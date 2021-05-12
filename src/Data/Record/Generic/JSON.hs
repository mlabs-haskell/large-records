{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Generic conversion to/from JSON
module Data.Record.Generic.JSON
  ( gtoJSON,
    gparseJSON,
  )
where

import Data.Aeson.Types
  ( FromJSON,
    Parser,
    ToJSON (toJSON),
    Value,
    object,
    withObject,
    (.:),
  )
import Data.HashMap.Strict (HashMap)
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Record.Generic
  ( Constraints,
    Dict (Dict),
    Generic (dict, from, metadata, to),
    I (I),
    K (K),
    Metadata (Metadata),
    Rep (Rep),
    mapKKK,
    recordFieldNames,
    recordName,
    unI,
  )
import qualified Data.Record.Generic.Rep as Rep
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Exts (Any)

gtoJSON ::
  forall (a :: Type).
  (Generic a, Constraints a ToJSON) =>
  a ->
  Value
gtoJSON =
  object
    . Rep.collapse
    . Rep.zipWith (mapKKK $ \n x -> (Text.pack n, x)) recordFieldNames
    . Rep.cmap (Proxy @ToJSON) (K . toJSON . unI)
    . from
  where
    Metadata {..} = metadata (Proxy @a)

gparseJSON ::
  forall (a :: Type).
  (Generic a, Constraints a FromJSON) =>
  Value ->
  Parser a
gparseJSON = withObject (recordName md) go
  where
    md :: Metadata a
    md = metadata @a Proxy
    constraints :: Vector (Dict FromJSON Any)
    Rep constraints = dict @a $ Proxy @FromJSON
    fields :: Vector (K String Any)
    Rep fields = recordFieldNames @a md
    go :: HashMap Text Value -> Parser a
    go obj = to . Rep <$> (traverse (decode obj) . V.zip fields $ constraints)
    decode ::
      HashMap Text Value ->
      (K String Any, Dict FromJSON Any) ->
      Parser (I Any)
    decode obj (K k, Dict) = I <$> obj .: Text.pack k
