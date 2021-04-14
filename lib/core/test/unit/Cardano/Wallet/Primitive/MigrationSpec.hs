{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Primitive.MigrationSpec
    where

import Prelude

import Cardano.Wallet.Primitive.Migration
    ( categorizeUTxOEntries
    , categorizeUTxOEntry
    , CategorizedUTxO (..)
    , createPlan
    , uncategorizeUTxOEntries
    , MigrationPlan (..)
    , UTxOEntryCategory (..)
    )
--import Data.Map.Strict
--    ( Map )
--import Data.Maybe
--    ( catMaybes )
import Cardano.Wallet.Primitive.Migration.Selection
    ( Selection (..)
    )
import Data.Either
    ( isLeft, isRight )
import Cardano.Wallet.Primitive.Migration.SelectionSpec
    ( MockTxConstraints (..)
    , MockInputId
    , genMockTxConstraints
    , genMockInput
    , genCoinRange
    , unMockTxConstraints
    , counterexampleMap
    )
--import Data.List.NonEmpty
--    ( NonEmpty (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( txOutputHasValidSize )
import Control.Monad
    ( replicateM )
import Data.Set
    ( Set )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Core.QuickCheck
    ( modifyMaxSuccess )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Property
    , Blind (..)
    , Gen
    , choose
    , Arbitrary (..)
    , oneof
    , conjoin
    , property
    , checkCoverage
    , cover
    , counterexample
    , suchThat
    , (===)
    )
import Data.Generics.Labels
    ()

import qualified Cardano.Wallet.Primitive.Migration.Selection as Selection
--import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
--import qualified Data.Map.Strict as Map

spec :: Spec
spec = describe "Cardano.Wallet.Primitive.MigrationSpec" $

    modifyMaxSuccess (const 1000) $ do

    parallel $ describe "Creating migration plans" $ do

        it "prop_createPlan" $
            property prop_createPlan

    parallel $ describe "Categorizing UTxO entries" $ do

        it "prop_categorizeUTxOEntry" $
            property prop_categorizeUTxOEntry

--------------------------------------------------------------------------------
-- Creating migration plans
--------------------------------------------------------------------------------

data MockCreatePlanArguments = MockCreatePlanArguments
    { mockConstraints :: MockTxConstraints
    , mockInputs :: [(MockInputId, TokenBundle)]
    , mockRewardBalance :: Coin
    }
    deriving (Eq, Show)

instance Arbitrary MockCreatePlanArguments where
    arbitrary = genMockCreatePlanArguments

genMockCreatePlanArguments :: Gen MockCreatePlanArguments
genMockCreatePlanArguments = do
    mockConstraints <- genMockTxConstraints
    mockInputCount <- choose (0, 100)
    mockInputs <- replicateM mockInputCount genMockInput
    mockRewardBalance <- oneof
        [ pure (Coin 0)
        , genCoinRange (Coin 1) (Coin 1_000_000)
        ]
    pure MockCreatePlanArguments
        { mockConstraints
        , mockInputs
        , mockRewardBalance
        }

prop_createPlan :: Blind MockCreatePlanArguments -> Property
prop_createPlan mockArgs =
    checkCoverage $
    counterexample counterexampleText $
    cover 0.1 (selectionCount == 1)
        "selectionCount == 1" $
    cover 0.1 (selectionCount == 2)
        "selectionCount == 2" $
    conjoin
        [ inputIds === inputIdsSelected `Set.union` inputIdsNotSelected
        , totalFee result === totalFeeExpected
        , initiators (unselected result) === []
        ]
  where
    Blind MockCreatePlanArguments
        { mockConstraints
        , mockInputs
        , mockRewardBalance
        } = mockArgs
    constraints = unMockTxConstraints mockConstraints
    result = createPlan constraints categorizedUTxO mockRewardBalance

    categorizedUTxO = categorizeUTxOEntries constraints mockInputs

    inputIds :: Set MockInputId
    inputIds = Set.fromList (fst <$> mockInputs)

    inputIdsSelected :: Set MockInputId
    inputIdsSelected = Set.fromList
        [ i
        | s <- selections result
        , (i, _) <- NE.toList (inputs s)
        ]

    inputIdsNotSelected :: Set MockInputId
    inputIdsNotSelected = Set.fromList
        $ fmap fst
        $ uncategorizeUTxOEntries
        $ unselected result

    selectionCount = length (selections result)

    totalFeeExpected :: Coin
    totalFeeExpected = F.foldMap fee (selections result)

    counterexampleText = counterexampleMap
        [ ( "mockConstraints"
          , show mockConstraints )
        , ( "count of initiators available"
          , show (length $ initiators categorizedUTxO) )
        , ( "count of initiators not selected"
          , show (length $ initiators $ unselected result) )
        , ( "count of supporters available"
          , show (length $ supporters categorizedUTxO) )
        , ( "count of supporters not selected"
          , show (length $ supporters $ unselected result) )
        , ( "count of freeriders available"
          , show (length $ freeriders categorizedUTxO) )
        , ( "count of freeriders not selected"
          , show (length $ freeriders $ unselected result) )
        , ( "count of ignorables available"
          , show (length $ ignorables categorizedUTxO) )
        , ( "count of ignorables not selected"
          , show (length $ ignorables $ unselected result) )
        ]

--------------------------------------------------------------------------------
-- Categorizing UTxO entries
--------------------------------------------------------------------------------

data MockCategorizeUTxOEntryArguments = MockCategorizeUTxOEntryArguments
    { mockConstraints :: MockTxConstraints
    , mockEntry :: (MockInputId, TokenBundle)
    }
    deriving (Eq, Show)

instance Arbitrary MockCategorizeUTxOEntryArguments where
    arbitrary = genMockCategorizeUTxOEntryArguments

genMockCategorizeUTxOEntryArguments :: Gen MockCategorizeUTxOEntryArguments
genMockCategorizeUTxOEntryArguments = do
    mockConstraints <- genMockTxConstraints
    let constraints = unMockTxConstraints mockConstraints
    mockEntry <- genMockInput `suchThat`
        (txOutputHasValidSize constraints . snd)
    pure MockCategorizeUTxOEntryArguments
        { mockConstraints
        , mockEntry
        }

prop_categorizeUTxOEntry :: MockCategorizeUTxOEntryArguments -> Property
prop_categorizeUTxOEntry mockArgs =
    checkCoverage $
    cover 1 (result == Initiator) "Initiator" $
    cover 1 (result == Supporter) "Supporter" $
    cover 1 (result == Freerider) "Freerider" $
    cover 0 (result == Ignorable) "Ignorable" $
    property $ case result of
        Initiator ->
            isRight $ Selection.create constraints (Coin 0) [mockEntry]
        Supporter ->
            isLeft $ Selection.create constraints (Coin 0) [mockEntry]
        Freerider ->
            isLeft $ Selection.create constraints (Coin 0) [mockEntry]
        Ignorable ->
            isLeft $ Selection.create constraints (Coin 0) [mockEntry]
  where
    MockCategorizeUTxOEntryArguments
        { mockConstraints
        , mockEntry
        } = mockArgs
    (_mockInputId, mockInputBundle) = mockEntry
    constraints = unMockTxConstraints mockConstraints
    result = categorizeUTxOEntry constraints mockInputBundle

--------------------------------------------------------------------------------
-- Miscellaneous types and functions
--------------------------------------------------------------------------------

coinToInteger :: Coin -> Integer
coinToInteger = fromIntegral . unCoin