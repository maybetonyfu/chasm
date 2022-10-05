{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Namable where
import RIO
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax

class WithName a where
    getNames :: a -> [String]

instance WithName (Pat a) where 
    getNames (PVar _ name) = getNames name

instance WithName (Name a) where
    getNames (Ident _ name) = [name]
    getNames (Symbol _ name) = [name]

instance (WithName a) => WithName [a] where
    getNames = concatMap getNames

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
