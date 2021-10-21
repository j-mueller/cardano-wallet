{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Api.GenSpec (spec) where

import Prelude

import Cardano.Api
    ( CardanoEra (..)
    , TxIn (..)
    , TxInsCollateral (..)
    , TxIx (..)
    , collateralSupportedInEra
    )
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
    , forAll
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
            describe "genTxInsCollateral" $ do
                it "ByronEra" $
                    property
                    $ forAll (genTxInsCollateral ByronEra)
                    $ genTxInCollateralCoverage ByronEra
                it "ShelleyEra" $
                    property
                    $ forAll (genTxInsCollateral ShelleyEra)
                    $ genTxInCollateralCoverage ShelleyEra
                it "AllegraEra" $
                    property
                    $ forAll (genTxInsCollateral AllegraEra)
                    $ genTxInCollateralCoverage AllegraEra
                it "MaryEra" $
                    property
                    $ forAll (genTxInsCollateral MaryEra)
                    $ genTxInCollateralCoverage MaryEra
                it "AlonzoEra" $
                    property
                    $ forAll (genTxInsCollateral AlonzoEra)
                    $ genTxInCollateralCoverage AlonzoEra

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

genTxInCollateralCoverage :: CardanoEra era -> TxInsCollateral era -> Property
genTxInCollateralCoverage era collateral =
    case collateralSupportedInEra era of
        Nothing ->
            (collateral == TxInsCollateralNone)
            & label ("collateral is never generated in " <> show era)
            & counterexample ("collateral was generated in " <> show era)
        Just _ ->
            checkCoverage
            $ cover 10 (hasNoCollateral collateral)
                "no collateral"
            $ cover 10 (hasSomeCollateral collateral)
                "some collateral"
            $ cover 2 (collateralLength collateral == Just 0)
                "list of zero collateral"
            $ cover 10 (collateralLength collateral > Just 0)
                "list of more than zero collateral"
            $ cover 10 (collateralLength collateral > Just 3)
                "list of more than three collateral"
            $ True

    where
        hasNoCollateral = (== TxInsCollateralNone)

        hasSomeCollateral = \case
            TxInsCollateralNone -> False
            TxInsCollateral _ _ -> True

        collateralLength = \case
            TxInsCollateralNone  -> Nothing
            TxInsCollateral _ cs -> Just $ length cs
