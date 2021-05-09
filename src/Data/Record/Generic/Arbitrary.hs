{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Record.Generic.Arbitrary (garbitrary) where

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Record.Generic (Constraints, Dict (Dict), Generic (dict, to), I (I), Rep (Rep))
import Data.Vector (Vector)
import GHC.Exts (Any)
import Test.QuickCheck (Gen)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))

-- | Generic implementation of the 'arbitrary' method.
--
-- Typical usage:
--
-- > instance Arbitrary T where
-- >    arbitrary = garbitrary
-- >    shrink = gshrink
garbitrary :: forall (a :: Type). (Generic a, Constraints a Arbitrary) => Gen a
garbitrary = to . Rep <$> traverse go arbRep
  where
    arbRep :: Vector ((Dict Arbitrary) Any)
    (Rep arbRep) = dict @a Proxy
    go :: Dict Arbitrary Any -> Gen (I Any)
    go Dict = I <$> arbitrary
