{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import Language.Haskell.Exts
import RIO

data Range = Global | Scoped SrcSpan deriving (Show)

data LoadVars = All | Inc [Text] | Exc [Text] deriving (Show)

type BasicInfo = (FilePath, FilePath)

data Load = Load
  { loadName :: Text,
    loadLoc :: SrcSpan,
    loadQual :: Bool,
    loadAs :: Maybe Text,
    loadVars :: LoadVars
  }
  deriving (Show)

data Slice = Slice
  { slRange :: Range,
    slSymbols :: [String],
    slModuleName :: Text,
    slId :: Int
  }
  deriving (Show)

data Bottle = Bottle
  { bottleName :: Text,
    bottlePath :: FilePath,
    drops :: [(Text, Text)]
  }
  deriving (Show)
