{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Namable where

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import RIO

class (Show a) => WithName a where
  getNames :: a -> [String]
instance (Show a) => WithName (Name a) where
  getNames (Ident _ name) = [name]
  getNames (Symbol _ name) = [name]

instance (WithName a, Show a) => WithName [a] where
  getNames = concatMap getNames

instance (Show a) => WithName (Pat a) where
  getNames (PVar _ name) = getNames name
  getNames (PInfixApp _ p1 _ p2) = getNames p1 ++ getNames p2
  getNames (PApp _ _ ps) = getNames ps
  getNames (PTuple _ _ ps) = getNames ps
  getNames (PList _ ps) = getNames ps
  getNames (PParen _ p) = getNames p
  getNames (PAsPat _ name p) = getNames name ++ getNames p
  getNames PLit {} = []
  getNames PWildCard {} = []
  getNames p = error $ "Node type not supported: " ++ show p


class (Show a) => WithSingleName a where
  getSingleName :: a -> String

instance (Show a) => WithSingleName (Name a) where
  getSingleName (Ident _ name) = name
  getSingleName (Symbol _ name) = name

