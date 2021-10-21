{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Api.GenSpec (spec) where

import Prelude

import Cardano.Api
    ( TxIn (..), TxIx (..) )
import Cardano.Api.Gen
import Data.Function
    ( (&) )
import Data.Word
    ( Word32 )
import Test.Hspec
import Test.QuickCheck
    ( Arbitrary
    , Property
    , arbitrary
    , checkCoverage
    , counterexample
    , cover
    , label
    , property
    )

spec :: Spec
spec =
    describe "Cardano.Api.Gen" $
        describe "Generator coverage" $ do
            it "genTxIx" $
                property genTxIxCoverage
            it "genTxIn" $
                property genTxInCoverage

genTxIxCoverage :: TxIx -> Property
genTxIxCoverage txIx = checkCoverage
    $ cover 1 (txIx == TxIx 0)
        "txIx is zero"
    $ cover 2 (txIx >= veryLargeTxIx)
        "txIx is very large"
    $ cover 10 (txIx > TxIx 0 && txIx < veryLargeTxIx)
        "txIx is between smallest and very large"
    $ label "no txIx is negative" (txIx >= TxIx 0)
      & counterexample "txIx was negative"
    where
        veryLargeTxIx :: TxIx
        veryLargeTxIx = TxIx $ fromInteger $ toInteger (maxBound :: Word32)

instance Arbitrary TxIx where
    arbitrary = genTxIndex

genTxInCoverage :: TxIn -> Property
genTxInCoverage (TxIn _id ix) =
    -- We don't provide any coverage for genShelleyHash, and so we don't provide
    -- any coverage for txId either (as txId consists of a shelleyHash).
    genTxIxCoverage ix

instance Arbitrary TxIn where
    arbitrary = genTxIn
