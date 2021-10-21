{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Api.GenSpec (spec) where

import Prelude

import Cardano.Api
    ( CardanoEra (..)
    , Lovelace
    , ScriptValidity (..)
    , SlotNo (..)
    , TxExtraKeyWitnesses (..)
    , TxFee (..)
    , TxIn (..)
    , TxInsCollateral (..)
    , TxIx (..)
    , TxScriptValidity (..)
    , TxValidityLowerBound (..)
    , TxValidityUpperBound (..)
    , collateralSupportedInEra
    , extraKeyWitnessesSupportedInEra
    , txFeesExplicitInEra
    , txScriptValiditySupportedInCardanoEra
    , validityLowerBoundSupportedInEra
    , validityUpperBoundSupportedInEra
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
    , conjoin
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
            it "genLovelace" $
                property genLovelaceCoverage
            describe "genTxFee" $ do
                it "ByronEra" $
                    property
                    $ forAll (genTxFee ByronEra)
                    $ genTxFeeCoverage ByronEra
                it "ShelleyEra" $
                    property
                    $ forAll (genTxFee ShelleyEra)
                    $ genTxFeeCoverage ShelleyEra
                it "AllegraEra" $
                    property
                    $ forAll (genTxFee AllegraEra)
                    $ genTxFeeCoverage AllegraEra
                it "MaryEra" $
                    property
                    $ forAll (genTxFee MaryEra)
                    $ genTxFeeCoverage MaryEra
                it "AlonzoEra" $
                    property
                    $ forAll (genTxFee AlonzoEra)
                    $ genTxFeeCoverage AlonzoEra
            it "genTtl" $
                property genTtlCoverage
            describe "genTxValidityLowerBound" $ do
                it "ByronEra" $
                    property
                    $ forAll (genTxValidityLowerBound ByronEra)
                    $ genTxValidityLowerBoundCoverage ByronEra
                it "ShelleyEra" $
                    property
                    $ forAll (genTxValidityLowerBound ShelleyEra)
                    $ genTxValidityLowerBoundCoverage ShelleyEra
                it "AllegraEra" $
                    property
                    $ forAll (genTxValidityLowerBound AllegraEra)
                    $ genTxValidityLowerBoundCoverage AllegraEra
                it "MaryEra" $
                    property
                    $ forAll (genTxValidityLowerBound MaryEra)
                    $ genTxValidityLowerBoundCoverage MaryEra
                it "AlonzoEra" $
                    property
                    $ forAll (genTxValidityLowerBound AlonzoEra)
                    $ genTxValidityLowerBoundCoverage AlonzoEra
            describe "genTxValidityUpperBound" $ do
                it "ByronEra" $
                    property
                    $ forAll (genTxValidityUpperBound ByronEra)
                    $ genTxValidityUpperBoundCoverage ByronEra
                it "ShelleyEra" $
                    property
                    $ forAll (genTxValidityUpperBound ShelleyEra)
                    $ genTxValidityUpperBoundCoverage ShelleyEra
                it "AllegraEra" $
                    property
                    $ forAll (genTxValidityUpperBound AllegraEra)
                    $ genTxValidityUpperBoundCoverage AllegraEra
                it "MaryEra" $
                    property
                    $ forAll (genTxValidityUpperBound MaryEra)
                    $ genTxValidityUpperBoundCoverage MaryEra
                it "AlonzoEra" $
                    property
                    $ forAll (genTxValidityUpperBound AlonzoEra)
                    $ genTxValidityUpperBoundCoverage AlonzoEra
            describe "genTxValidityRangeBound" $ do
                it "ByronEra" $
                    property
                    $ forAll (genTxValidityRange ByronEra)
                    $ genTxValidityRangeCoverage ByronEra
                it "ShelleyEra" $
                    property
                    $ forAll (genTxValidityRange ShelleyEra)
                    $ genTxValidityRangeCoverage ShelleyEra
                it "AllegraEra" $
                    property
                    $ forAll (genTxValidityRange AllegraEra)
                    $ genTxValidityRangeCoverage AllegraEra
                it "MaryEra" $
                    property
                    $ forAll (genTxValidityRange MaryEra)
                    $ genTxValidityRangeCoverage MaryEra
                it "AlonzoEra" $
                    property
                    $ forAll (genTxValidityRange AlonzoEra)
                    $ genTxValidityRangeCoverage AlonzoEra
            it "genScriptValidity" $
                property genScriptValidityCoverage
            describe "genTxScriptValidity" $ do
                it "ByronEra" $
                    property
                    $ forAll (genTxScriptValidity ByronEra)
                    $ genTxScriptValidityCoverage ByronEra
                it "ShelleyEra" $
                    property
                    $ forAll (genTxScriptValidity ShelleyEra)
                    $ genTxScriptValidityCoverage ShelleyEra
                it "AllegraEra" $
                    property
                    $ forAll (genTxScriptValidity AllegraEra)
                    $ genTxScriptValidityCoverage AllegraEra
                it "MaryEra" $
                    property
                    $ forAll (genTxScriptValidity MaryEra)
                    $ genTxScriptValidityCoverage MaryEra
                it "AlonzoEra" $
                    property
                    $ forAll (genTxScriptValidity AlonzoEra)
                    $ genTxScriptValidityCoverage AlonzoEra
            describe "genExtraKeyWitnesses" $ do
                it "ByronEra" $
                    property
                    $ forAll (genExtraKeyWitnesses ByronEra)
                    $ genExtraKeyWitnessesCoverage ByronEra
                it "ShelleyEra" $
                    property
                    $ forAll (genExtraKeyWitnesses ShelleyEra)
                    $ genExtraKeyWitnessesCoverage ShelleyEra
                it "AllegraEra" $
                    property
                    $ forAll (genExtraKeyWitnesses AllegraEra)
                    $ genExtraKeyWitnessesCoverage AllegraEra
                it "MaryEra" $
                    property
                    $ forAll (genExtraKeyWitnesses MaryEra)
                    $ genExtraKeyWitnessesCoverage MaryEra
                it "AlonzoEra" $
                    property
                    $ forAll (genExtraKeyWitnesses AlonzoEra)
                    $ genExtraKeyWitnessesCoverage AlonzoEra

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
            collateral == TxInsCollateralNone
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
              True

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

genLovelaceCoverage :: Lovelace -> Property
genLovelaceCoverage = unsignedCoverage "lovelace"

instance Arbitrary Lovelace where
    arbitrary = genLovelace

genTxFeeCoverage :: CardanoEra era -> TxFee era -> Property
genTxFeeCoverage era fee =
    case txFeesExplicitInEra era of
        Left implicit ->
            fee == TxFeeImplicit implicit
            & label ("fee in " <> show era <> " is always implicit")
            & counterexample ( "fee in era "
                              <> show era
                              <> " wasn't implicit but should be"
                             )
        Right _ ->
            case fee of
                TxFeeImplicit _ ->
                    error "fees are explicit in era but received implicit fee"
                TxFeeExplicit _ l ->
                    genLovelaceCoverage l

genTtlCoverage :: SlotNo -> Property
genTtlCoverage = genSlotNoCoverage

genTxValidityLowerBoundCoverage
    :: CardanoEra era -> TxValidityLowerBound era -> Property
genTxValidityLowerBoundCoverage era validFrom =
    case validityLowerBoundSupportedInEra era of
        Nothing ->
            validFrom == TxValidityNoLowerBound
            & label ("validity lower bound not supported in " <> show era)
            & counterexample ( "validity lower bound shouldn't be supported in "
                              <> show era
                             )
        Just _ ->
            case validFrom of
                TxValidityNoLowerBound ->
                    False
                    & counterexample ( "validity lower bound supported in "
                                       <> show era
                                       <> ", should have lower bound")
                TxValidityLowerBound _ ttl ->
                    genTtlCoverage ttl

genTxValidityUpperBoundCoverage
    :: CardanoEra era -> TxValidityUpperBound era -> Property
genTxValidityUpperBoundCoverage era validFrom =
    case validityUpperBoundSupportedInEra era of
        Nothing ->
            case validFrom of
                (TxValidityNoUpperBound _) ->
                    True
                    & label ( "validity upper bound not supported in "
                              <> show era
                            )
                    & counterexample
                        ( "validity upper bound shouldn't be supported in "
                          <> show era
                        )
                (TxValidityUpperBound _ _) ->
                    error ( "validity upper bound not supported in "
                           <> show era
                           <> ", no upper bound should be generated."
                          )
        Just _ ->
            case validFrom of
                TxValidityNoUpperBound _ ->
                    False
                    & counterexample ( "validity upper bound supported in "
                                       <> show era
                                       <> ", should have upper bound")
                TxValidityUpperBound _ ttl ->
                    genTtlCoverage ttl

genTxValidityRangeCoverage
    :: CardanoEra era
    -> (TxValidityLowerBound era, TxValidityUpperBound era)
    -> Property
genTxValidityRangeCoverage era (lower, upper) = conjoin
    [ genTxValidityLowerBoundCoverage era lower
    , genTxValidityUpperBoundCoverage era upper
    ]

genScriptValidityCoverage :: ScriptValidity -> Property
genScriptValidityCoverage scriptValidity = checkCoverage
    $ cover 40 (scriptValidity == ScriptInvalid)
        "script is invalid"
    $ cover 40 (scriptValidity == ScriptValid)
        "script is valid"
    $ True

instance Arbitrary ScriptValidity where
    arbitrary = genScriptValidity

genTxScriptValidityCoverage :: CardanoEra era -> TxScriptValidity era -> Property
genTxScriptValidityCoverage era txScriptValidity =
    case txScriptValiditySupportedInCardanoEra era of
        Nothing ->
            txScriptValidity == TxScriptValidityNone
            & label "script validity is always none in eras it is not supported"
            & counterexample
                ("script validity was generated in unsupported " <> show era)
        Just _  ->
            case txScriptValidity of
                TxScriptValidityNone ->
                    False
                    & counterexample
                        (show era <> "era should have script validity")
                TxScriptValidity _ scriptValidity ->
                    genScriptValidityCoverage scriptValidity

genExtraKeyWitnessesCoverage
    :: CardanoEra era -> TxExtraKeyWitnesses era -> Property
genExtraKeyWitnessesCoverage era ws =
    case extraKeyWitnessesSupportedInEra era of
        Nothing ->
            label "extra key witnesses are not generated in unsupported eras"
                (noWitnesses ws)
            & counterexample ( "key witnesses were generated in unsupported "
                               <> show era
                             )
        Just _ -> checkCoverage
            $ cover 10 (noWitnesses ws)
                "no witnesses"
            $ cover 10 (witnesses ws)
                "witnesses"
            $ case ws of
                TxExtraKeyWitnessesNone -> property True
                (TxExtraKeyWitnesses _ wits) -> checkCoverage
                    $ cover 1 (null wits)
                       "empty witneses"
                    $ cover 30 (not (null wits))
                       "some witnesses"
                    $ cover 10 (length wits > 3)
                       "> 3 witnesses"
                    $ True

    where
        noWitnesses = (== TxExtraKeyWitnessesNone)

        witnesses = \case
            TxExtraKeyWitnessesNone -> False
            TxExtraKeyWitnesses _ _ -> True

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
