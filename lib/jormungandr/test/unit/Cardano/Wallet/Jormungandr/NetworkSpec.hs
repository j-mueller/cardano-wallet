{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Jormungandr.NetworkSpec
    ( spec
    , noLog
    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Api.Client
    ( JormungandrClient (..) )
import Cardano.Wallet.Jormungandr.BlockHeaders
    ( emptyBlockHeaders )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr, Network (Testnet) )
import Cardano.Wallet.Jormungandr.Network
    ( ErrGetDescendants (..), mkRawNetworkLayer )
import Cardano.Wallet.Network
    ( Cursor, ErrGetBlock (..), NetworkLayer (..), NextBlocksResult (..) )
import Cardano.Wallet.Primitive.Model
    ( BlockchainParameters (..) )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..), Hash (..), SlotId (..) )
import Control.Concurrent.MVar.Lifted
    ( newMVar )
import Control.Monad.Fail
    ( MonadFail )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Control
    ( MonadBaseControl )
import Control.Monad.Trans.Except
    ( except, runExceptT, throwE )
import Control.Monad.Trans.State.Lazy
    ( StateT (..), evalStateT, get, modify', put, runStateT )
import Data.Coerce
    ( coerce )
import Data.Functor
    ( ($>) )
import Data.List
    ( (\\) )
import Data.Map
    ( Map )
import Data.Maybe
    ( catMaybes, fromMaybe )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word32 )
import Safe
    ( headMay, lastMay )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , NonNegative (..)
    , Property
    , choose
    , counterexample
    , frequency
    , property
    , sublistOf
    , vectorOf
    , withMaxSuccess
    )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )

import qualified Cardano.Wallet.Jormungandr.Binary as J
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map

{-------------------------------------------------------------------------------
                                      Spec
-------------------------------------------------------------------------------}

spec :: Spec
spec = do
    describe "Chain sync" $ do
        it "Syncs with mock node"
            $ withMaxSuccess 1000
            $ property prop_sync

{-------------------------------------------------------------------------------
                Syncing of network layer in presence of rollback
                          and concurrent node updates.
-------------------------------------------------------------------------------}

prop_sync :: S -> Property
prop_sync initialNode = monadicIO $ do
    let logLineN msg = logLine $ "[PRODUCER] " <> msg
    let logLineC msg = logLine $ "[CONSUMER] " <> msg

    -- Set up network layer with mock Jormungandr
    nl <- run $ evalStateT (mockNetworkLayer logLineN) initialNode

    -- Run a model chain consumer on the mock network layer.
    let initialConsumer = C [] (initCursor nl block0H) 10
    let restoreStep = consumerRestoreStep logLineC nl initialConsumer
    (consumer, s) <- run $ runStateT restoreStep initialNode

    monitor $ counterexample $ unlines
        [ "Applied blocks: " <> showChain (consumerApplied consumer)
        , "Node chain:     " <> showChain (getNodeChain (node s))
        , "Logs:         \n" <> unlines (reverse (logs s))
        ]
    -- Consumer chain should (eventually) be in sync with node chain.
    assert (consumerApplied consumer == getNodeChain (node s))
  where
    logLine msg = modify' (\(S a0 a1 a2 logs) -> S a0 a1 a2 (msg:logs))

showChain :: [MockBlock] -> String
showChain [] = "∅"
showChain chain = unwords . map (showHash . mockBlockId) $ chain

-- | Test Genesis block
block0H :: BlockHeader
block0H = BlockHeader (SlotId 0 0) (Quantity 0) (Hash "genesis")

----------------------------------------------------------------------------
-- Model consumer

-- | Model of consumer state.
data Consumer = C
    { consumerApplied :: [MockBlock]
    -- ^ Blocks which have been received.
    , _consumerCursor :: Cursor (Jormungandr 'Testnet)
    -- ^ Consumer state -- the cursor given by network layer.
    , _consumerStepsRemaining :: Int
    -- ^ Counter which prevents infinite loops in tests.
    }

-- | A model consumer that repeatedly fetches blocks and "applies" them by
-- appending to its chain. Returns the new 'Consumer' state.
-- This is similar to the Cardano.Wallet worker restore loop.
consumerRestoreStep
    :: (Monad m)
    => (String -> StateT S m ())
    -- ^ logger function
    -> TestNetworkLayer m
    -- ^ Network layer.
    -> Consumer
    -- ^ Current consumer state.
    -> StateT S m Consumer
consumerRestoreStep logLine nw c@(C bs cur iLimit)
    | iLimit > 0 = do
        runExceptT (nextBlocks nw cur) >>= \case
            Left e ->
                logLine ("Failed to get next blocks: " ++ show e) $> c
            Right AwaitReply -> do
                logLine "AwaitReply"
                consumerRestoreStep logLine nw (C bs cur (min i 3))
            Right (RollForward cur' h bs') -> do
                logLine $ "RollFoward: " <> show h
                consumerRestoreStep logLine nw (C (bs ++ bs') cur' i)
            Right (RollBackward cur') -> do
                logLine "RollBackward"
                let sl = cursorSlotId nw cur'
                let bs' = takeWhile (\b -> mockBlockSlot b <= sl) bs
                consumerRestoreStep logLine nw (C bs' cur' i)
    | otherwise = pure c -- don't loop forever
  where
    i = iLimit - 1

----------------------------------------------------------------------------
-- Network layer with mock jormungandr node

type TestNetworkLayer m =
    NetworkLayer (StateT S m) (Jormungandr 'Testnet) MockBlock

-- | Instantiate new network layer with mock jormungandr.
mockNetworkLayer
    :: forall m. (MonadFail m, MonadBaseControl IO m)
    => (String -> StateT S m ()) -- ^ logger function
    -> StateT S m (TestNetworkLayer m)
mockNetworkLayer logLine = do
    let jm = mockJormungandrClient logLine
    st <- newMVar emptyBlockHeaders
    Right g0 <- runExceptT $ getInitialBlockchainParameters jm (Hash "genesis")
    pure $ fromJBlock <$> mkRawNetworkLayer g0 st jm

-- | A network layer which returns mock blocks and mutates its state according
-- to the generated operations.
mockJormungandrClient
    :: Monad m
    => (String -> StateT S m ())
        -- ^ logger function
    -> JormungandrClient (StateT S m)
mockJormungandrClient logLine = JormungandrClient
    { getTipId = do
        ch <- nodeChainIds <$> lift getNodeState
        lift applyOp
        let tip = fromMaybe (Hash "genesis") $ headMay ch
        lift . logLine $ "getTipId" <> returns tip
        pure tip

    , getBlock = \blockId -> do
        bs <- nodeDb <$> lift getNodeState
        lift applyOp
        let block = if blockId == Hash "genesis"
                then pure $ toJBlock $ MockBlock blockId (Hash "") (SlotId 0 0) 0
                else case Map.lookup blockId bs of
                    Just b -> pure $ toJBlock b
                    Nothing -> Left $ ErrGetBlockNotFound blockId
        lift . logLine $ "getBlock " <> show blockId <> returns block
        except block

    , getDescendantIds = \parentId count -> do
        lift . logLine $ "getDescendentIds " ++ show parentId ++ " " ++ show count
        ch <- nodeChainIds <$> lift getNodeState
        lift applyOp
        case takeWhile (/= parentId) (reverse ch) of
            [] -> throwE $ ErrGetDescendantsParentNotFound parentId
            ds -> pure $ take (fromIntegral count) ds

    , getInitialBlockchainParameters = \blockId -> do
        Quantity k <- mockNodeK <$> lift get
        let block0 = MockBlock (coerce blockId) (Hash "") (SlotId 0 0) 0
        pure (toJBlock block0, BlockchainParameters
            { getGenesisBlockHash = blockId
            , getGenesisBlockDate = error "mock bp"
            , getFeePolicy = error "mock bp"
            , getSlotLength = error "mock bp"
            , getEpochLength = error "mock bp"
            , getTxMaxSize = error "mock bp"
            , getEpochStability = Quantity (fromIntegral k)
            })

    , postMessage = \_ -> error "mock postMessage"
    , getStakeDistribution = error "mock getStakeDistribution"
    }
  where
    returns :: Show a => a -> String
    returns a = "\n    ↳  " <> show a

    getNodeState = node <$> get
    -- Consume and apply one operation
    applyOp = do
        s <- get
        case s of
            S _ [] _ _ ->
                return ()
            S n (op:ops) k logs -> do
                logLine (show op)
                put (S (applyNodeOp op n) ops k logs)

-- If debugging, you might want to log 'ByteString' or 'Text'. Don't use putStrLn
-- on 'String :: [Char]' because characters of output will be interleaved between
-- concurrent threads. Otherwise, here is a log function that does nothing.
noLog :: Monad m => String -> m ()
noLog = const (pure ())

----------------------------------------------------------------------------
-- Model node

-- | State of the mock node which is used by 'mockJormungandrClient'.
--
-- Jormungandr is a concurrent process to the wallet. Its state can change in
-- arbitrary ways between any REST API call.
data S = S
    { node :: Node
    -- ^ Node's current state.
    , operations :: [NodeOp]
    -- ^ Future mutations to apply to the node.
    -- They are consumed and applied in after each REST API call.
    , mockNodeK :: Quantity "block" Word32
    -- ^ Maximum number of unstable blocks.
    , logs :: [String]
    -- ^ Logs from the test execution
    } deriving (Show, Eq)

-- | Models the jormungandr node state.
data Node = N
    { nodeDb :: Map (Hash "BlockHeader") MockBlock
    -- ^ Blocks indexed by id.
    , nodeChainIds :: [Hash "BlockHeader"]
    -- ^ List of ids for the chain, newest first.
    , nodeCounter :: Int
    -- ^ Counter used to generate block ids.
    } deriving (Show, Eq)

-- | Gets the mock node's chain as a list of blocks starting from genesis.
getNodeChain :: Node -> [MockBlock]
getNodeChain (N db ch _) = reverse $ catMaybes [Map.lookup b db | b <- ch]

getNodeTip :: Node -> Maybe MockBlock
getNodeTip (N db ch _) = lastMay ch >>= flip Map.lookup db

-- | Mutation of the node state.
data NodeOp
    = NodeAddBlocks [MockBlock]
    | NodeRewind Int
    | NodeGarbageCollect [Hash "BlockHeader"]
    deriving (Show, Eq)

applyNodeOp :: NodeOp -> Node -> Node
applyNodeOp (NodeAddBlocks bs) = nodeAddBlocks bs
applyNodeOp (NodeRewind i) = nodeRewind i
applyNodeOp (NodeGarbageCollect hs) = nodeGarbageCollect hs

-- | Add blocks to the node chain.
nodeAddBlocks :: [MockBlock] -> Node -> Node
nodeAddBlocks bs n = N db' chain' nodeCounter'
  where
    chain' = reverse (map mockBlockId bs) ++ nodeChainIds n
    db' = nodeDb n <> Map.fromList [(mockBlockId b, b) | b <- bs]
    nodeCounter' = nodeCounter n + length bs

-- | Roll back, or remove some blocks from the node chain. They are not removed
-- from the DB yet (see 'nodeGarbageCollect').
nodeRewind :: Int -> Node -> Node
nodeRewind i n = n { nodeChainIds = take (length (nodeChainIds n) - i) (nodeChainIds n) }

-- | "Garbage collect" models the potential that getting rolled back blocks will
-- still succeed after the chain is switched, but that it will stop succeeding
-- at some unknown point in the future.
nodeGarbageCollect :: [Hash "BlockHeader"] -> Node -> Node
nodeGarbageCollect hs (N bs c x) = N bs' c x
    where bs' = foldr Map.delete bs hs

----------------------------------------------------------------------------
-- Mock block

-- | A block which can be easily generated, shown, and used to assert
-- properties.
data MockBlock = MockBlock
    { mockBlockId :: Hash "BlockHeader"
    , mockBlockPrev :: Hash "BlockHeader"
    , mockBlockSlot :: SlotId
    , mockBlockContent :: Int
    } deriving (Show, Eq)

-- | Stuffs a mock block into a real block.
toJBlock :: MockBlock -> J.Block
toJBlock (MockBlock bid prev sl content) = J.Block hdr []
    where hdr = J.BlockHeader 0 0 sl (fromIntegral content) (coerce bid) prev Nothing

-- | Extract a mock block out of a real block.
fromJBlock :: J.Block -> MockBlock
fromJBlock (J.Block (J.BlockHeader _ _ sl content bid prev _) _) =
    MockBlock (coerce bid) prev sl (fromIntegral content)

----------------------------------------------------------------------------
-- Generation of mock node test cases.

instance Arbitrary S where
    arbitrary = do
        NonNegative count <- arbitrary
        let node = N mempty mempty 0
        mockNodeK <- arbitrary
        operations <- genNodeOps mockNodeK count node []
        let logs = []
        pure $ S{..}
      where
        -- Repeatedly generate operations then update state.
        -- The state is used so that only valid operations are generated.
        genNodeOps :: Quantity "block" Word32 -> Int -> Node -> [NodeOp] -> Gen [NodeOp]
        genNodeOps _ 0 _ ac = pure $ reverse ac
        genNodeOps k count n ac = do
            op <- genNodeOp k n
            genNodeOps k (count - 1) (applyNodeOp op n) (op:ac)

        -- Given a node state generate a valid mutation.
        -- fixme: use 'sized' to scale rollback, etc.
        genNodeOp :: Quantity "block" Word32 -> Node -> Gen NodeOp
        genNodeOp (Quantity k) n@(N _ ch _) = frequency
                [ (10, NodeAddBlocks <$> genBlocks n)
                , (3, NodeRewind <$> choose (1, (min (fromIntegral k) (length ch))))
                , (1, NodeGarbageCollect <$> genGC n)
                ]

        genBlocks :: Node -> Gen [MockBlock]
        genBlocks n = do
            count <- choose (1, 12) -- fixme: getSize would be better
            let genEmpty = frequency [(1, pure True), (4, pure False)]
            empty <- vectorOf count genEmpty
            let tip = getNodeTip n
            let tipSlot = maybe (-1) (fromIntegral . slotNumber . mockBlockSlot) tip
            let slots = [ SlotId 0 (fromIntegral $ tipSlot + i)
                        | (i, gap) <- zip [1..count] empty, not gap ]
            let ixs = [nodeCounter n ..]
            let bids = map mockBlockHash ixs
            let prev = maybe (Hash "genesis") (mockBlockHash . mockBlockContent) tip
            pure [ MockBlock bid p sl ix
                 | ((bid, p), (ix, sl)) <- zip (zip bids (prev:bids)) (zip ixs slots) ]

        genGC :: Node -> Gen [Hash "BlockHeader"]
        genGC (N db ch _) = sublistOf (Map.keys db \\ ch)

    shrink (S n ops k _) = [S n ops' k [] | ops' <- shrink ops]

instance Arbitrary (Quantity "block" Word32) where
    -- k doesn't need to be large for testing this
    arbitrary =
        Quantity . fromIntegral <$> choose (2 :: Int, 20)
    shrink (Quantity k) =
        [ Quantity (fromIntegral k')
        | k' <- shrink (fromIntegral k :: Int)
        , k' >= 2
        ]

-- Arbitrary instance is actually just for shrinking.
instance Arbitrary NodeOp where
    arbitrary = pure $ NodeAddBlocks []
    shrink (NodeAddBlocks bs) =
        [ NodeAddBlocks (take s bs) | s <- shrink (length bs), s > 0 ]
    shrink (NodeRewind n) = NodeRewind <$> shrink n
    shrink (NodeGarbageCollect hs) = NodeGarbageCollect <$> shrink hs

instance Arbitrary MockBlock where
    arbitrary = pure $ MockBlock (Hash "") (Hash "") (SlotId 0 0) 0

instance Arbitrary (Hash "BlockHeader")  where
    arbitrary = mockBlockHash . getNonNegative <$> arbitrary

mockBlockHash :: Int -> Hash "BlockHeader"
mockBlockHash num = Hash. B8.pack $ "block" ++ show num

showHash :: Hash a -> String
showHash (Hash h) = B8.unpack h
