{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Api.Gen
  ( genTxIn
  , genTxId
  , genTxIndex
  , genShelleyHash
  , genTxInsCollateral
  , genSlotNo
  ) where

import Prelude

import Cardano.Api hiding
    ( txIns )
import Test.QuickCheck
    ( Gen, Large (..), arbitrary, listOf, oneof )

import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.Hash as Crypto
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
