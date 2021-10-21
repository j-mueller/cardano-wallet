{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Api.Gen
  ( genTxIn
  , genTxId
  , genTxIndex
  , genShelleyHash
  , genTxInsCollateral
  , genSlotNo
  , genLovelace
  , genTxFee
  , genTtl
  , genTxValidityLowerBound
  , genTxValidityUpperBound
  , genTxValidityRange
  , genTxScriptValidity
  , genScriptValidity
  , genSeed
  , genSigningKey
  , genVerificationKey
  , genVerificationKeyHash
  , genExtraKeyWitnesses
  , genSimpleScript
  , genPlutusScript
  , genScript
  , genScriptInAnyLang
  , genScriptInEra
  , genScriptHash
  ) where

import Prelude

import Cardano.Api hiding
    ( txIns )
import Cardano.Api.Shelley
    ( PlutusScript (..) )
import Data.Maybe
    ( maybeToList )
import Data.Word
    ( Word64 )
import Test.Cardano.Crypto.Gen
    ()
import Test.QuickCheck
    ( Gen
    , Large (..)
    , Positive (..)
    , arbitrary
    , choose
    , elements
    , frequency
    , listOf
    , oneof
    , scale
    , sized
    , vector
    )

import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.Seed as Crypto
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import qualified Shelley.Spec.Ledger.TxBody as Ledger
    ( EraIndependentTxBody )

genShelleyHash
    :: Gen (Crypto.Hash Crypto.Blake2b_256 Ledger.EraIndependentTxBody)
genShelleyHash = return . Crypto.castHash $ Crypto.hashWith CBOR.serialize' ()

genTxIn :: Gen TxIn
genTxIn = TxIn <$> genTxId <*> genTxIndex

genTxId :: Gen TxId
genTxId = TxId <$> genShelleyHash

genTxIndex :: Gen TxIx
genTxIndex = do
    (Large (n :: Word)) <- arbitrary
    pure $ TxIx n

genTxInsCollateral :: CardanoEra era -> Gen (TxInsCollateral era)
genTxInsCollateral era =
    case collateralSupportedInEra era of
      Nothing        -> pure TxInsCollateralNone
      Just supported -> oneof
                          [ pure TxInsCollateralNone
                          , TxInsCollateral supported <$> listOf genTxIn
                          ]

genSlotNo :: Gen SlotNo
genSlotNo = SlotNo <$> arbitrary

genLovelace :: Gen Lovelace
genLovelace = do
    (Large (n :: Word64)) <- arbitrary
    pure $ quantityToLovelace $ Quantity $ toInteger n

genTxFee :: CardanoEra era -> Gen (TxFee era)
genTxFee era =
  case txFeesExplicitInEra era of
    Left  implicit -> pure (TxFeeImplicit implicit)
    Right explicit -> TxFeeExplicit explicit <$> genLovelace

genTtl :: Gen SlotNo
genTtl = genSlotNo

genTxValidityLowerBound :: CardanoEra era -> Gen (TxValidityLowerBound era)
genTxValidityLowerBound era =
  case validityLowerBoundSupportedInEra era of
    Nothing        -> pure TxValidityNoLowerBound
    Just supported -> TxValidityLowerBound supported <$> genTtl

genTxValidityUpperBound :: CardanoEra era -> Gen (TxValidityUpperBound era)
genTxValidityUpperBound era =
  case (validityUpperBoundSupportedInEra era,
       validityNoUpperBoundSupportedInEra era) of
    (Just supported, _) ->
      TxValidityUpperBound supported <$> genTtl

    (Nothing, Just supported) ->
      pure (TxValidityNoUpperBound supported)

    (Nothing, Nothing) ->
      error "genTxValidityUpperBound: unexpected era support combination"

genTxValidityRange
  :: CardanoEra era
  -> Gen (TxValidityLowerBound era, TxValidityUpperBound era)
genTxValidityRange era =
  (,)
    <$> genTxValidityLowerBound era
    <*> genTxValidityUpperBound era

genTxScriptValidity :: CardanoEra era -> Gen (TxScriptValidity era)
genTxScriptValidity era = case txScriptValiditySupportedInCardanoEra era of
  Nothing -> pure TxScriptValidityNone
  Just witness -> TxScriptValidity witness <$> genScriptValidity

genScriptValidity :: Gen ScriptValidity
genScriptValidity = elements [ScriptInvalid, ScriptValid]

genSeed :: Int -> Gen Crypto.Seed
genSeed n = (Crypto.mkSeedFromBytes . BS.pack) <$> vector n

genSigningKey :: Key keyrole => AsType keyrole -> Gen (SigningKey keyrole)
genSigningKey roletoken = do
    seed <- genSeed (fromIntegral seedSize)
    let sk = deterministicSigningKey roletoken seed
    return sk

    where
        seedSize :: Word
        seedSize = deterministicSigningKeySeedSize roletoken

genVerificationKey
    :: Key keyrole
    => AsType keyrole
    -> Gen (VerificationKey keyrole)
genVerificationKey roletoken = getVerificationKey <$> genSigningKey roletoken

genVerificationKeyHash :: Key keyrole => AsType keyrole -> Gen (Hash keyrole)
genVerificationKeyHash roletoken =
  verificationKeyHash <$> genVerificationKey roletoken

genExtraKeyWitnesses :: CardanoEra era -> Gen (TxExtraKeyWitnesses era)
genExtraKeyWitnesses era =
    case extraKeyWitnessesSupportedInEra era of
        Nothing -> pure TxExtraKeyWitnessesNone
        Just supported  -> oneof
            [ pure TxExtraKeyWitnessesNone
            , TxExtraKeyWitnesses supported
              <$> listOf (genVerificationKeyHash AsPaymentKey)
            ]

genPlutusScript :: PlutusScriptVersion lang -> Gen (PlutusScript lang)
genPlutusScript _ =
    -- We make no attempt to create a valid script
    PlutusScriptSerialised . SBS.toShort <$> arbitrary

genSimpleScript :: SimpleScriptVersion lang -> Gen (SimpleScript lang)
genSimpleScript lang =
    sized genTerm
  where
    genTerm 0 = oneof nonRecursive
    genTerm n = frequency
        [ (3, oneof (recursive n))
        , (1, oneof nonRecursive)
        ]

    -- Non-recursive generators
    nonRecursive =
        (RequireSignature . verificationKeyHash <$>
            genVerificationKey AsPaymentKey)

      : [ RequireTimeBefore supported <$> genSlotNo
        | supported <- maybeToList (timeLocksSupported lang) ]

     ++ [ RequireTimeAfter supported <$> genSlotNo
        | supported <- maybeToList (timeLocksSupported lang) ]

    -- Recursive generators
    recursive n =
        [ RequireAllOf <$> scale (`mod` 10) (listOf $ recurse n)

        , RequireAnyOf <$> scale (`mod` 10) (listOf $ recurse n)

        , do ts <- scale (`mod` 10) $ listOf $ recurse n
             m  <- choose (0, length ts)
             return (RequireMOf m ts)
        ]

    recurse n = do
        (Positive m) <- arbitrary
        genTerm (n `div` (m + 3))

genScript :: ScriptLanguage lang -> Gen (Script lang)
genScript (SimpleScriptLanguage lang) =
    SimpleScript lang <$> genSimpleScript lang
genScript (PlutusScriptLanguage lang) =
    PlutusScript lang <$> genPlutusScript lang

genScriptInAnyLang :: Gen ScriptInAnyLang
genScriptInAnyLang =
    oneof
      [ ScriptInAnyLang lang <$> genScript lang
      | AnyScriptLanguage lang <- [minBound..maxBound] ]

genScriptInEra :: CardanoEra era -> Gen (ScriptInEra era)
genScriptInEra era =
    oneof
      [ ScriptInEra langInEra <$> genScript lang
      | AnyScriptLanguage lang <- [minBound..maxBound]
      , Just langInEra <- [scriptLanguageSupportedInEra era lang] ]

genScriptHash :: Gen ScriptHash
genScriptHash = do
    ScriptInAnyLang _ script <- genScriptInAnyLang
    return (hashScript script)
