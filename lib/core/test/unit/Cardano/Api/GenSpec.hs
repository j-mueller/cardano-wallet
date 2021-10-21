{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Api.GenSpec (spec) where

import Prelude

import Cardano.Api
    ( CardanoEra (..)
    , SlotNo (..)
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
            it "genSlotNo" $
                property genSlotNoCoverage

genTxIxCoverage :: TxIx -> Property
genTxIxCoverage (TxIx ix) = unsignedCoverage "txIx" ix

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

genSlotNoCoverage :: SlotNo -> Property
genSlotNoCoverage = unsignedCoverage "slot number"

instance Arbitrary SlotNo where
    arbitrary = genSlotNo

unsignedCoverage
    :: ( Num a
       , Ord a
       )
    => String
    -> a
    -> Property
unsignedCoverage name x = checkCoverage
    $ cover 1 (x == 0)
        (name <> " is zero")
    $ cover 30 (x > 0 && x < veryLarge)
        (name <> " is between zero and very large")
    $ cover 5 (x > veryLarge)
        (name <> " is greater than very large")
    $ label (name <> " is non-negative") (x >= 0)
      & counterexample (name <> " was negative")

    where
        veryLarge = fromIntegral (maxBound :: Word32)
