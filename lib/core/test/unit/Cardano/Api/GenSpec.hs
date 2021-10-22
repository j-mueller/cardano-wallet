{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Api.GenSpec (spec) where

import Prelude

import Cardano.Api
    ( AnyScriptLanguage (..)
    , AssetId (..)
    , AssetName (..)
    , BuildTx
    , CardanoEra (..)
    , ExecutionUnits (..)
    , KeyWitnessInCtx (..)
    , Lovelace
    , NetworkId (..)
    , NetworkMagic (..)
    , Quantity (..)
    , ScriptData (..)
    , ScriptValidity (..)
    , SimpleScript (..)
    , SimpleScriptVersion (..)
    , SlotNo (..)
    , TxAuxScripts (..)
    , TxExtraKeyWitnesses (..)
    , TxFee (..)
    , TxIn (..)
    , TxInsCollateral (..)
    , TxIx (..)
    , TxMetadata (..)
    , TxMetadataInEra (..)
    , TxMetadataValue (..)
    , TxMintValue (..)
    , TxScriptValidity (..)
    , TxValidityLowerBound (..)
    , TxValidityUpperBound (..)
    , TxWithdrawals (..)
    , Value
    , WitCtxStake
    , Witness (..)
    , auxScriptsSupportedInEra
    , collateralSupportedInEra
    , extraKeyWitnessesSupportedInEra
    , multiAssetSupportedInEra
    , scriptLanguageSupportedInEra
    , txFeesExplicitInEra
    , txMetadataSupportedInEra
    , txScriptValiditySupportedInCardanoEra
    , validityLowerBoundSupportedInEra
    , validityUpperBoundSupportedInEra
    , valueToList
    , withdrawalsSupportedInEra
    )
import Cardano.Api.Gen
import Cardano.Api.Shelley
    ( StakeCredential (..) )
import Data.ByteString
    ( ByteString )
import Data.Char
    ( isAlphaNum, isDigit, isLower, isUpper )
import Data.Function
    ( (&) )
import Data.Int
    ( Int32 )
import Data.Proxy
    ( Proxy (..), asProxyTypeOf )
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

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map

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
            it "genSimpleScriptV1" $
                property genSimpleScriptCoverageV1
            it "genSimpleScriptV2" $
                property genSimpleScriptCoverageV2
            it "genAssetName" $
                property genAssetNameCoverage
            it "genAlphaNum" $
                property genAlphaNumCoverage
            it "genAssetId" $
                property genAssetIdCoverage
            it "genValueForMinting" $
                property
                $ forAll genValueForMinting genValueForMintingCoverage
            it "genSignedQuantity" $
                property genSignedQuantityCoverage
            describe "genTxMintValue" $ do
                it "genTxMintValue ByronEra" $
                    property
                    $ forAll (genTxMintValue ByronEra)
                    $ genTxMintValueCoverage ByronEra
                it "genTxMintValue ShelleyEra" $
                    property
                    $ forAll (genTxMintValue ShelleyEra)
                    $ genTxMintValueCoverage ShelleyEra
                it "genTxMintValue AllegraEra" $
                    property
                    $ forAll (genTxMintValue AllegraEra)
                    $ genTxMintValueCoverage AllegraEra
                it "genTxMintValue MaryEra" $
                    property
                    $ forAll (genTxMintValue MaryEra)
                    $ genTxMintValueCoverage MaryEra
                it "genTxMintValue AlonzoEra" $
                    property
                    $ forAll (genTxMintValue AlonzoEra)
                    $ genTxMintValueCoverage AlonzoEra
            it "genNetworkMagic" $
                property genNetworkMagicCoverage
            it "genNetworkId" $
                property genNetworkIdCoverage
            it "genStakeCredential" $
                property genStakeCredentialCoverage
            it "genScriptDataCoverage" $
                property genScriptDataCoverage
            it "genExecutionUnits" $
                property genExecutionUnitsCoverage
            describe "genWitnessStake" $ do
                it "genWitnessStake ByronEra" $
                    property
                    $ forAll (genWitnessStake ByronEra)
                    $ genWitnessStakeCoverage ByronEra
                it "genWitnessStake ShelleyEra" $
                    property
                    $ forAll (genWitnessStake ShelleyEra)
                    $ genWitnessStakeCoverage ShelleyEra
                it "genWitnessStake AllegraEra" $
                    property
                    $ forAll (genWitnessStake AllegraEra)
                    $ genWitnessStakeCoverage AllegraEra
                it "genWitnessStake MaryEra" $
                    property
                    $ forAll (genWitnessStake MaryEra)
                    $ genWitnessStakeCoverage MaryEra
                it "genWitnessStake AlonzoEra" $
                    property
                    $ forAll (genWitnessStake AlonzoEra)
                    $ genWitnessStakeCoverage AlonzoEra
            describe "genTxWithdrawals" $ do
                it "genTxWithdrawals ByronEra" $
                    property
                    $ forAll (genTxWithdrawals ByronEra)
                    $ genTxWithdrawalsCoverage ByronEra
                it "genTxWithdrawals ShelleyEra" $
                    property
                    $ forAll (genTxWithdrawals ShelleyEra)
                    $ genTxWithdrawalsCoverage ShelleyEra
                it "genTxWithdrawals AllegraEra" $
                    property
                    $ forAll (genTxWithdrawals AllegraEra)
                    $ genTxWithdrawalsCoverage AllegraEra
                it "genTxWithdrawals MaryEra" $
                    property
                    $ forAll (genTxWithdrawals MaryEra)
                    $ genTxWithdrawalsCoverage MaryEra
                it "genTxWithdrawals AlonzoEra" $
                    property
                    $ forAll (genTxWithdrawals AlonzoEra)
                    $ genTxWithdrawalsCoverage AlonzoEra
            describe "genTxAuxScripts" $ do
                it "genTxAuxScripts ByronEra" $
                    property
                    $ forAll (genTxAuxScripts ByronEra)
                    $ genTxAuxScriptsCoverage ByronEra
                it "genTxAuxScripts ShelleyEra" $
                    property
                    $ forAll (genTxAuxScripts ShelleyEra)
                    $ genTxAuxScriptsCoverage ShelleyEra
                it "genTxAuxScripts AllegraEra" $
                    property
                    $ forAll (genTxAuxScripts AllegraEra)
                    $ genTxAuxScriptsCoverage AllegraEra
                it "genTxAuxScripts MaryEra" $
                    property
                    $ forAll (genTxAuxScripts MaryEra)
                    $ genTxAuxScriptsCoverage MaryEra
                it "genTxAuxScripts AlonzoEra" $
                    property
                    $ forAll (genTxAuxScripts AlonzoEra)
                    $ genTxAuxScriptsCoverage AlonzoEra
            it "genTxMetadataValue" $
                property genTxMetadataValueCoverage
            it "genTxMetadata" $
                property genTxMetadataCoverage
            describe "genTxMetadataInEra" $ do
                it "genTxMetadataInEra ByronEra" $
                    property
                    $ forAll (genTxMetadataInEra ByronEra)
                    $ genTxMetadataInEraCoverage ByronEra
                it "genTxMetadataInEra ShelleyEra" $
                    property
                    $ forAll (genTxMetadataInEra ShelleyEra)
                    $ genTxMetadataInEraCoverage ShelleyEra
                it "genTxMetadataInEra AllegraEra" $
                    property
                    $ forAll (genTxMetadataInEra AllegraEra)
                    $ genTxMetadataInEraCoverage AllegraEra
                it "genTxMetadataInEra MaryEra" $
                    property
                    $ forAll (genTxMetadataInEra MaryEra)
                    $ genTxMetadataInEraCoverage MaryEra
                it "genTxMetadataInEra AlonzoEra" $
                    property
                    $ forAll (genTxMetadataInEra AlonzoEra)
                    $ genTxMetadataInEraCoverage AlonzoEra

genTxIxCoverage :: TxIx -> Property
genTxIxCoverage (TxIx ix) = unsignedCoverage (Proxy @Word32) "txIx" ix

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
genSlotNoCoverage = unsignedCoverage (Proxy @Word32) "slot number"

instance Arbitrary SlotNo where
    arbitrary = genSlotNo

genLovelaceCoverage :: Lovelace -> Property
genLovelaceCoverage = unsignedCoverage (Proxy @Word32) "lovelace"

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
        True

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
            $ cover 10 (noWitnesses ws) "no witnesses"
            $ cover 10 (witnesses ws) "witnesses"
            $ case ws of
                TxExtraKeyWitnessesNone -> property True
                (TxExtraKeyWitnesses _ wits) -> checkCoverage
                    $ cover 1 (null wits) "empty witneses"
                    $ cover 30 (not (null wits)) "some witnesses"
                    $ cover 10 (length wits > 3) "> 3 witnesses" True

    where
        noWitnesses = (== TxExtraKeyWitnessesNone)

        witnesses = \case
            TxExtraKeyWitnessesNone -> False
            TxExtraKeyWitnesses _ _ -> True

genSimpleScriptCoverageV1 :: Property
genSimpleScriptCoverageV1 =
    forAll (genSimpleScript SimpleScriptV1) $ \script ->
    checkCoverage
    $ cover 10 (requiresSignature script)
        "script has \"require signature\""
    $ cover 10 (requiresAllOf script)
        "script has \"require all of\""
    $ cover 10 (requiresAnyOf script)
        "script has \"require any of\""
    $ cover 10 (requiresMOf script)
        "script has \"require M of\""
    $ cover 10 (hasOnlyNonRecursiveScriptPrimitive script)
        "script has only non-recursive script primitives (sig, timeBefore, timeAfter)"
    $ cover 10 (not (hasOnlyNonRecursiveScriptPrimitive script))
        "script has recursive script primitives (allOf, anyOf, mOf)"
        True

genSimpleScriptCoverageV2 :: Property
genSimpleScriptCoverageV2 =
    forAll (genSimpleScript SimpleScriptV2) $ \script ->
    checkCoverage
    $ cover 10 (requiresSignature script)
        "script has \"require signature\""
    $ cover 10 (requiresAllOf script)
        "script has \"require all of\""
    $ cover 10 (requiresAnyOf script)
        "script has \"require any of\""
    $ cover 10 (requiresMOf script)
        "script has \"require M of\""
    $ cover 10 (requiresTimeBefore script)
        "script has \"time before\""
    $ cover 10 (requiresTimeAfter script)
        "script has \"time after\""
    $ cover 10 (hasOnlyNonRecursiveScriptPrimitive script)
        "script has only non-recursive script primitives (sig, timeBefore, timeAfter)"
    $ cover 10 (not (hasOnlyNonRecursiveScriptPrimitive script))
        "script has recursive script primitives (allOf, anyOf, mOf)"
        True

hasOnlyNonRecursiveScriptPrimitive :: forall lang. SimpleScript lang -> Bool
hasOnlyNonRecursiveScriptPrimitive = \case
    (RequireSignature _)    -> True
    (RequireTimeBefore _ _) -> True
    (RequireTimeAfter _ _)  -> True
    (RequireAllOf _)        -> False
    (RequireAnyOf _)        -> False
    (RequireMOf _ _)        -> False

-- Returns true if any part of the script might require a signature.
requiresSignature :: forall lang. SimpleScript lang -> Bool
requiresSignature = \case
    (RequireSignature _)    -> True
    (RequireTimeBefore _ _) -> False
    (RequireTimeAfter _ _)  -> False
    (RequireAllOf ss)       -> any requiresSignature ss
    (RequireAnyOf ss)       -> any requiresSignature ss
    (RequireMOf _ ss)       -> any requiresSignature ss

-- Returns true if any part of the script requires "time before".
requiresTimeBefore :: forall lang. SimpleScript lang -> Bool
requiresTimeBefore = \case
    (RequireSignature _)    -> False
    (RequireTimeBefore _ _) -> True
    (RequireTimeAfter _ _)  -> False
    (RequireAllOf ss)       -> any requiresTimeBefore ss
    (RequireAnyOf ss)       -> any requiresTimeBefore ss
    (RequireMOf _ ss)       -> any requiresTimeBefore ss

-- Returns true if any part of the script requires "time after".
requiresTimeAfter :: forall lang. SimpleScript lang -> Bool
requiresTimeAfter = \case
    (RequireSignature _)    -> False
    (RequireTimeBefore _ _) -> False
    (RequireTimeAfter _ _)  -> True
    (RequireAllOf ss)       -> any requiresTimeBefore ss
    (RequireAnyOf ss)       -> any requiresTimeBefore ss
    (RequireMOf _ ss)       -> any requiresTimeBefore ss

requiresAllOf :: forall lang. SimpleScript lang -> Bool
requiresAllOf = \case
    (RequireSignature _)    -> False
    (RequireTimeBefore _ _) -> False
    (RequireTimeAfter _ _)  -> False
    (RequireAllOf _)        -> True
    (RequireAnyOf ss)       -> any requiresAllOf ss
    (RequireMOf _ ss)       -> any requiresAllOf ss

requiresAnyOf :: forall lang. SimpleScript lang -> Bool
requiresAnyOf = \case
    (RequireSignature _)    -> False
    (RequireTimeBefore _ _) -> False
    (RequireTimeAfter _ _)  -> False
    (RequireAllOf ss)       -> any requiresAnyOf ss
    (RequireAnyOf _)        -> True
    (RequireMOf _ ss)       -> any requiresAnyOf ss

requiresMOf :: forall lang. SimpleScript lang -> Bool
requiresMOf = \case
    (RequireSignature _)    -> False
    (RequireTimeBefore _ _) -> False
    (RequireTimeAfter _ _)  -> False
    (RequireAllOf ss)       -> any requiresMOf ss
    (RequireAnyOf ss)       -> any requiresMOf ss
    (RequireMOf _ _)        -> True

genAssetNameCoverage :: AssetName -> Property
genAssetNameCoverage n = checkCoverage
    $ cover 1 (assetNameLen == 0)
        "asset name is empty"
    $ cover 10 (assetNameLen == shortLength)
        "asset name is short"
    $ cover 5 (assetNameLen == longLength)
        "asset name is long"
    $ cover 5 (assetNameLen > shortLength && assetNameLen < longLength)
        "asset name is between short and long"
    $ label "is alphanumeric" (all isAlphaNum assetNameStr)
      & counterexample "character wasn't alphabetic or numeric"
    where
        assetNameStr = (\(AssetName n') -> B8.unpack n') n
        assetNameLen = (\(AssetName n') -> BS.length n') n
        shortLength = 1
        longLength = 32

instance Arbitrary AssetName where
    arbitrary = genAssetName

genAlphaNumCoverage :: Property
genAlphaNumCoverage = forAll genAlphaNum $ \c ->
    checkCoverage
    $ cover 10 (isDigit c)
        "character is digit"
    $ cover 10 (isUpper c)
        "character is upper-case alphabetic"
    $ cover 10 (isLower c)
        "character is lower-case alphabetic"
    $ label "character is alphabetic or numeric" (isAlphaNum c)
      & counterexample "character wasn't alphabetic or numeric"

genAssetIdCoverage :: AssetId -> Property
genAssetIdCoverage assetId = checkCoverage
    $ cover 10 (isAdaAssetId assetId)
        "ADA asset id"
    $ cover 10 (isNonAdaAssetId assetId)
        "non-ADA asset id"
        True

    where
        isAdaAssetId = (== AdaAssetId)

        isNonAdaAssetId AdaAssetId    = False
        isNonAdaAssetId (AssetId _ _) = True

instance Arbitrary AssetId where
    arbitrary = genAssetId

genValueForMintingCoverage :: Value -> Property
genValueForMintingCoverage val = checkCoverage
    $ cover 10 (hasMintingValue val)
      "minting assets"
    $ cover 10 (hasBurningValue val)
      "burning assets"
    $ conjoin
      [ label "no empty mint/burn" (not $ hasZeroValue val)
        & counterexample "shouldn't generate a zero mint/burn value"
      , label "is never ADA value (can't mint ADA)" (hasNoAdaValue val)
        & counterexample "generated ADA mint (you can't mint ADA!)"
      ]

    where
        hasNoAdaValue = all ((/= AdaAssetId) . fst) . valueToList

        hasMintingValue = any ((> 0) . snd) . valueToList

        hasBurningValue = any ((< 0) . snd) . valueToList

        hasZeroValue = any ((== 0) . snd) . valueToList

genSignedQuantityCoverage :: Quantity -> Property
genSignedQuantityCoverage = signedCoverage "quantity"

instance Arbitrary Quantity where
    arbitrary = genSignedQuantity

genTxMintValueCoverage :: CardanoEra era -> TxMintValue BuildTx era -> Property
genTxMintValueCoverage era val =
    case multiAssetSupportedInEra era of
        Left _ ->
            label "mint values are not generated in unsupported eras"
                (val == TxMintNone)
            & counterexample ( "a mint value was generated in an unsupported "
                              <> show era
                             )
        Right _ ->
            checkCoverage
            $ cover 40 (noMint val)
              "no mint"
            $ cover 40 (someMint val)
              "mint"
              True

    where
        noMint = (== TxMintNone)

        someMint = \case
            TxMintNone -> False
            TxMintValue {} -> True

genNetworkMagicCoverage :: NetworkMagic -> Property
genNetworkMagicCoverage (NetworkMagic n) =
    unsignedCoverage (Proxy @Int32) "network magic" n

instance Arbitrary NetworkMagic where
    arbitrary = genNetworkMagic

genNetworkIdCoverage :: NetworkId -> Property
genNetworkIdCoverage n = checkCoverage
    $ cover 10 (isMainnet n)
        "network is mainnet"
    $ cover 10 (isTestnet n)
        "network is testnet"
        True
    where
        isMainnet = (== Mainnet)

        isTestnet = \case
            Mainnet   -> False
            Testnet _ -> True

instance Arbitrary NetworkId where
    arbitrary = genNetworkId

genStakeCredentialCoverage :: StakeCredential -> Property
genStakeCredentialCoverage sc = checkCoverage
    $ cover 10 (isByKey sc)
        "stake credential built from key"
    $ cover 10 (isByScript sc)
        "stake credential built from script"
        True

    where
        isByKey = \case
            StakeCredentialByKey _    -> True
            StakeCredentialByScript _ -> False

        isByScript = \case
            StakeCredentialByKey _    -> False
            StakeCredentialByScript _ -> True

instance Arbitrary StakeCredential where
    arbitrary = genStakeCredential

genScriptDataNumberCoverage :: Integer -> Property
genScriptDataNumberCoverage n = checkCoverage
    $ cover 1 (n == 0)
        "number is equal to 0"
    $ cover 2 (n >= veryLargeNumber)
        "number is very large"
    $ cover 2 (n <= verySmallNumber)
        "number is very small"
    $ cover 10 (n > verySmallNumber && n < veryLargeNumber)
        "number is between very small and very large"
        True

    where
        verySmallNumber = fromIntegral (minBound :: Int32)
        veryLargeNumber = fromIntegral (maxBound :: Int32)

genScriptDataBytesCoverage :: ByteString -> Property
genScriptDataBytesCoverage bs = checkCoverage
    $ cover 1 (BS.length bs == 0)
        "no bytes"
    $ cover 10 (BS.length bs > 0)
        "some bytes"
    $ cover 2 (BS.length bs > 32)
        "lots of bytes"
        True

genScriptDataListCoverage :: [ScriptData] -> Property
genScriptDataListCoverage ss = checkCoverage
    $ cover 1 (null ss)
        "no scripts in list"
    $ cover 10 (not $ null ss)
        "some scripts in list"
    $ cover 10 (length ss > 32)
        "lots of scripts in list"
    $ conjoin
    $ fmap genScriptDataCoverage ss

genScriptDataMapCoverage :: [(ScriptData, ScriptData)] -> Property
genScriptDataMapCoverage ss = checkCoverage
    $ cover 1 (null ss)
        "no scripts in map"
    $ cover 10 (not $ null ss)
        "some scripts in map"
    $ cover 10 (length ss > 32)
        "lots of scripts in map"
    $ conjoin
    $ fmap (\(k, v) ->
                conjoin [genScriptDataCoverage k, genScriptDataCoverage v]
           ) ss

genScriptDataConstructorCoverage :: (Integer, [ScriptData]) -> Property
genScriptDataConstructorCoverage (ix, ss) = checkCoverage
    $ cover 1 (null ss)
        "no scripts in constr"
    $ cover 10 (not $ null ss)
        "some scripts in constr"
    $ cover 10 (length ss > 3)
        "lots of scripts in constr"
    $ cover 1 (ix == 0)
        "ix == 0"
    $ cover 10 (ix > 0)
        "ix > 0"
    $ cover 10 (ix > 3)
        "ix > 3"
        True

genScriptDataCoverage :: ScriptData -> Property
genScriptDataCoverage dat = conjoin
    [ checkCoverage
      $ case dat of
          ScriptDataNumber _        -> cover 10 True "is script data number"
          ScriptDataBytes _         -> cover 10 True "is script data bytes"
          ScriptDataList _          -> cover 10 True "is script data list"
          ScriptDataMap _           -> cover 10 True "is script data map"
          ScriptDataConstructor _ _ -> cover 10 True "is script data constructor"
      $ True
    , case dat of
          ScriptDataNumber n        ->
              genScriptDataNumberCoverage n
          ScriptDataBytes bs        ->
              genScriptDataBytesCoverage bs
          ScriptDataList ss          ->
              genScriptDataListCoverage ss
          ScriptDataMap ss           ->
              genScriptDataMapCoverage ss
          ScriptDataConstructor n ss ->
              genScriptDataConstructorCoverage (n, ss)
    ]

instance Arbitrary ScriptData where
    arbitrary = genScriptData

genExecutionUnitsCoverage :: ExecutionUnits -> Property
genExecutionUnitsCoverage (ExecutionUnits steps mem) = conjoin
    [ unsignedCoverage (Proxy @Word32) "execution steps" steps
    , unsignedCoverage (Proxy @Word32) "execution mem" mem
    ]

instance Arbitrary ExecutionUnits where
    arbitrary = genExecutionUnits

genWitnessStakeCoverage
    :: CardanoEra era
    -> Witness WitCtxStake era
    -> Property
genWitnessStakeCoverage era wit =
    let
        supportedLangs = [ ()
                         | AnyScriptLanguage lang <- [minBound..maxBound]
                         , Just _langInEra <-
                               [scriptLanguageSupportedInEra era lang]
                         ]
    in
        if null supportedLangs
        then
            wit == KeyWitness KeyWitnessForStakeAddr
            & label ("only key witnesses are generated in " <> show era)
            & counterexample ( "a script witness was generated in "
                              <> show era
                              <> " but only key witnesses are supported in this"
                              <> " era."
                             )
        else
            checkCoverage
            $ cover 20 (wit == KeyWitness KeyWitnessForStakeAddr)
                "key witnesses in era"
            $ cover 20 (isScriptWitness wit)
                "script witnesses in era"
                True

    where
        isScriptWitness = \case
            (KeyWitness _)      -> False
            (ScriptWitness _ _) -> True

genTxWithdrawalsCoverage
    :: CardanoEra era -> TxWithdrawals BuildTx era -> Property
genTxWithdrawalsCoverage era ws =
    case withdrawalsSupportedInEra era of
        Nothing ->
            ws == TxWithdrawalsNone
            & label "withdrawals not generated in unsupported eras"
            & counterexample ( "withdrawals were generated in unsupported era "
                               <> show era
                             )
        Just _ -> checkCoverage
            $ case ws of
                TxWithdrawalsNone -> cover 10 True "no withdrawals" True
                TxWithdrawals _ xs ->
                    cover 1 (null xs) "empty withdrawals"
                    $ cover 10 (not $ null xs) "some withdrawals"
                    $ cover 10 (length xs > 3) "more withdrawals"
                      True

genTxAuxScriptsCoverage :: CardanoEra era -> TxAuxScripts era -> Property
genTxAuxScriptsCoverage era aux =
    case auxScriptsSupportedInEra era of
        Nothing ->
            aux == TxAuxScriptsNone
            & label "aux scripts not generated in unsupported era"
            & counterexample ( "aux scripts were generated in unsupported "
                               <> show era
                             )
        Just _ -> checkCoverage
            $ case aux of
                TxAuxScriptsNone  -> cover 10 True "no aux scripts" True
                TxAuxScripts _ ss ->
                    cover 1 (null ss) "empty aux scripts"
                    $ cover 50 (not $ null ss) "non-empty aux scripts"
                    $ cover 30 (length ss > 3) "some aux scripts"
                      True

genTxMetadataValueCoverage :: TxMetadataValue -> Property
genTxMetadataValueCoverage meta =
    checkCoverage
        $ cover 10 (isMetaNumber meta) "is TxMetaNumber"
        $ cover 10 (isMetaBytes meta) "is TxMetaBytes"
        $ cover 10 (isMetaText meta) "is TxMetaText"
        $ cover 10 (isMetaList meta) "is TxMetaList"
        $ cover 10 (isMetaMap meta) "is TxMetaMap" True

    where
        isMetaNumber = \case
            TxMetaNumber _ -> True
            _              -> False

        isMetaBytes = \case
            TxMetaBytes _ -> True
            _             -> False

        isMetaText = \case
            TxMetaText _ -> True
            _            -> False

        isMetaList = \case
            TxMetaList _ -> True
            _            -> False

        isMetaMap = \case
            TxMetaMap _  -> True
            _            -> False

instance Arbitrary TxMetadataValue where
    arbitrary = genTxMetadataValue

genTxMetadataCoverage :: TxMetadata -> Property
genTxMetadataCoverage (TxMetadata meta) =
    let
        metaMap = Map.toList meta
    in
        checkCoverage
            $ cover 1 (null metaMap)
              "no metadata entries"
            $ cover 10 (not $ null metaMap)
              "some metadata entries"
            $ cover 10 (length metaMap > 10)
              "lots of metadata entries"
            $ conjoin $ fmap (metaNumberCoverage . fst) metaMap

    where
        metaNumberCoverage n = checkCoverage
            $ cover 1 (n == 0)
              "meta index == 0"
            $ cover 10 (n > 0)
              "meta index > 0"
            $ cover 10 (n >= veryLargeMetaIndex)
              "meta index is large"
            $ cover 10 (n > 0 && n < veryLargeMetaIndex)
              "meta index is between smallest and large"
            $ property (n >= 0)
              & counterexample "meta index was negative"

        veryLargeMetaIndex = fromIntegral (maxBound :: Word32)

instance Arbitrary TxMetadata where
    arbitrary = genTxMetadata

genTxMetadataInEraCoverage :: CardanoEra era -> TxMetadataInEra era -> Property
genTxMetadataInEraCoverage era meta =
    case txMetadataSupportedInEra era of
        Nothing ->
            meta == TxMetadataNone
            & label "metadata not generated in unsupported era"
            & counterexample ( "metadata was generated in unsupported "
                               <> show era
                             )
        Just _ -> checkCoverage
            $ case meta of
                TxMetadataNone -> cover 10 True "no metadata" True
                TxMetadataInEra _ _ -> cover 40 True "some metadata" True

-- | Provide coverage for an unsigned number.
unsignedCoverage
    :: ( Num a
       , Ord a
       , Bounded b
       , Integral b
       )
    => Proxy b
    -- ^ Numbers greater than the maxBound of this type are considered "large"
    -> String
    -- ^ The name of the entity we are providing coverage for
    -> a
    -- ^ The value of the entity
    -> Property
unsignedCoverage p name x = checkCoverage
    $ cover 1 (x == 0)
        (name <> " is zero")
    $ cover 30 (x > 0 && x < veryLarge)
        (name <> " is between zero and very large")
    $ cover 5 (x > veryLarge)
        (name <> " is greater than very large")
    $ label (name <> " is non-negative") (x >= 0)
      & counterexample (name <> " was negative")

    where
        veryLarge = fromIntegral $ maxBound `asProxyTypeOf` p

signedCoverage
  :: ( Num a
     , Ord a
     )
  => String
  -> a
  -> Property
signedCoverage name x = checkCoverage
    $ cover 1 (x == 0)
        (name <> " is zero")
    $ cover 30 (x > verySmall && x < veryLarge)
        (name <> " is between very small and very large")
    $ cover 5 (x > veryLarge)
        (name <> " is greater than very large")
    $ cover 5 (x < verySmall)
        (name <> " is less than very small")
        True

    where
        veryLarge = fromIntegral (maxBound :: Word32)
        verySmall = -fromIntegral (maxBound :: Word32)
