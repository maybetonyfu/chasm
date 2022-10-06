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
  getNames (PNPlusK _ name _) = getNames name
  getNames (PInfixApp _ p1 _ p2) = getNames p1 ++ getNames p2
  getNames (PApp _ _ ps) = getNames ps
  getNames (PTuple _ _ ps) = getNames ps
  getNames (PUnboxedSum _ _ _ p) = getNames p
  getNames (PList _ ps) = getNames ps
  getNames (PParen _ p) = getNames p
  -- getNames (PRec _ _ pfs) = concatMap getNames pfs
  getNames (PAsPat _ name p) = getNames name ++ getNames p
  getNames (PIrrPat _ p) = getNames p
  getNames (PatTypeSig _ p _) = getNames p
  getNames PLit {} = []
  getNames PWildCard {} = []
  getNames p = error $ "Node: " ++ show p

-- patternVarNames :: Show a => Pat a -> [String]
-- patternVarNames (PVar _ name) = [getName name]
-- patternVarNames (PNPlusK _ name _) = [getName name]
-- patternVarNames (PInfixApp _ p1 _ p2) = patternVarNames p1 ++ patternVarNames p2
-- patternVarNames (PApp _ _ ps) = concatMap patternVarNames ps
-- patternVarNames (PTuple _ _ ps) = concatMap patternVarNames ps
-- patternVarNames (PUnboxedSum _ _ _ p) = patternVarNames p
-- patternVarNames (PList _ ps) = concatMap patternVarNames ps
-- patternVarNames (PParen _ p) = patternVarNames p
-- patternVarNames (PRec _ _ pfs) = concatMap patternFieldVarNames pfs
-- patternVarNames (PAsPat _ name p) = getName name : patternVarNames p
-- patternVarNames (PIrrPat _ p) = patternVarNames p
-- patternVarNames (PatTypeSig _ p _) = patternVarNames p
-- patternVarNames PLit {} = []
-- patternVarNames PWildCard {} = []
-- patternVarNames p = error $ "Node: " ++ show p
