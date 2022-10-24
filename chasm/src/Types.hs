{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import Language.Haskell.Exts
import Language.Prolog (Term)
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
    slSymbols :: [Text],
    slModuleName :: Text,
    slId :: Int
  }
  deriving (Show)

data Bottle = Bottle
  { bottleName :: Text,
    bottlePath :: FilePath,
    bottleDrops :: [(Text, Type SrcSpanInfo)]
  }
  deriving (Show)


data Constraint = Constraint
  { cstId :: Int,
    cstHead :: String,
    cstBody :: Term
  } deriving (Show)

instance Eq Constraint where
  c1 == c2 = cstId c1 == cstId c2

