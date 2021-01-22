{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
{-# OPTIONS_GHC -fplugin=GhcDump.Plugin        #-}

module Test.Record.Generic.Size.Before.R020 where

import Data.Aeson
import Generics.SOP.JSON
import Generics.SOP.TH

import Test.Record.Generic.Size.Infra

-- @RecordDotPreprocessor@ runs before TH, so cannot use 'recordOfSize'.
data R = MkR {
      -- 1 .. 10
      field1  :: T 1
    , field2  :: T 2
    , field3  :: T 3
    , field4  :: T 4
    , field5  :: T 5
    , field6  :: T 6
    , field7  :: T 7
    , field8  :: T 8
    , field9  :: T 9
    , field10 :: T 10
      -- 11 .. 20
    , field11 :: T 11
    , field12 :: T 12
    , field13 :: T 13
    , field14 :: T 14
    , field15 :: T 15
    , field16 :: T 16
    , field17 :: T 17
    , field18 :: T 18
    , field19 :: T 19
    , field20 :: T 20
    }

deriving instance Eq   R
deriving instance Show R

deriveGeneric ''R

instance ToJSON R where
  toJSON = gtoJSON defaultJsonOptions
