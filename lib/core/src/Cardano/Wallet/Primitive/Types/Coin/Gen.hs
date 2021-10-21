{-# LANGUAGE NumericUnderscores #-}
module Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoin
    , genCoinPositive
    , genCoinFullRange
    , shrinkCoin
    , shrinkCoinPositive
    , shrinkCoinFullRange
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Test.QuickCheck
    ( Gen, choose, frequency, shrink, sized )

--------------------------------------------------------------------------------
-- Coins chosen according to the size parameter.
--------------------------------------------------------------------------------

genCoin :: Gen Coin
genCoin = frequency
    [ (1, pure (Coin 0))
    , (5, Coin <$> choose (1_000_000, 1_000_000_000_000))
    ]


shrinkCoin :: Coin -> [Coin]
shrinkCoin (Coin c) = Coin <$> shrink c

--------------------------------------------------------------------------------
-- Coins chosen according to the size parameter, but strictly positive.
--------------------------------------------------------------------------------

genCoinPositive :: Gen Coin
genCoinPositive = sized $ \n -> Coin . fromIntegral <$> choose (1, max 1 n)

shrinkCoinPositive :: Coin -> [Coin]
shrinkCoinPositive (Coin c) = Coin <$> filter (> 0) (shrink c)

--------------------------------------------------------------------------------
-- Coins chosen from the full range available.
--------------------------------------------------------------------------------

-- | Generates coins across the full range available.
--
-- This generator has a slight bias towards the limits of the range, but
-- otherwise generates values uniformly across the whole range.
--
-- This can be useful when testing roundtrip conversions between different
-- types.
--
genCoinFullRange :: Gen Coin
genCoinFullRange = frequency
    [ (1, pure (Coin 0))
    , (1, pure (maxBound :: Coin))
    , (1, Coin <$> choose (1000000, 1000000000))
    , (8, Coin <$> choose (1, unCoin (maxBound :: Coin) - 1))
    ]

shrinkCoinFullRange :: Coin -> [Coin]
shrinkCoinFullRange =
    -- Given that we may have a large value, we limit the number of results
    -- returned in order to avoid processing long lists of shrunken values.
    take 8 . shrinkCoin
