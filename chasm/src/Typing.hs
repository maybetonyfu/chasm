{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Typing where

import Data.String.QQ
import qualified RIO.Text as T
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import Namable
import RIO
import RIO.List
import Range
import Lenses
import Types
import Goal
import Var
import qualified Language.Prolog

type Term = Language.Prolog.Term

logShow :: (Show a, HasLogFunc env) => a -> RIO env ()
logShow  = logInfo . displayShow

fresh :: (HasTypeVarCounter env) => RIO env Term
fresh = do
  typeVarCounterHandle <- view typeVarCounterL
  value <- readIORef typeVarCounterHandle
  modifyIORef typeVarCounterHandle (+1)
  return (varFrom ("Fresh_" ++ show value))

assignVar :: (HasSlices env) =>  SrcSpanInfo -> String -> RIO env Term
assignVar srcspan v = do
  slices <- readIORefFromLens slicesL
  let mbslice = findSliceByRange slices (T.pack v) srcspan
  case mbslice of
    Nothing -> error ("Cannot find symbol: " ++ v)
    Just slice -> do
        let vId = show (slId slice)
        let vMod = T.unpack (slModuleName slice)
        return (varFrom ("Var_" ++ vMod ++ "_" ++ v ++ "_" ++ vId))

assignQualVar :: (HasSlices env) => String -> String -> RIO env Term
assignQualVar m v = do
  slices <- readIORefFromLens slicesL
  let mbslice = findSliceByModname slices (T.pack v) (T.pack m)
  case mbslice of
    Nothing -> error ("Cannot find symbol: " ++ v)
    Just slice -> do
        let vId = show (slId slice)
        let vMod = T.unpack (slModuleName slice)
        return (varFrom ("Var_" ++ vMod ++ "_" ++ v ++ "_" ++ vId))


class HasTyping f where
  matchTerm :: (HasLogFunc env, HasTypeVarCounter env, HasSlices env) => Term -> f SrcSpanInfo -> RIO env ()

instance HasTyping Module where
  matchTerm t (Module _ _ _ _ decls) = mapM_ (matchTerm t) decls
  matchTerm _ _ = undefined

instance HasTyping Decl where
  matchTerm t (PatBind srcspan pat rhs maybeWheres) = do
    v <- fresh
    matchTerm v pat
    matchTerm v rhs

  matchTerm t (FunBind srcspan matches) = do
    mapM_ (matchTerm t) matches
  matchTerm t node = error ("Node type not support: " ++ show node)

instance HasTyping Pat where
  matchTerm t (PVar srcspan name) = do
    let names = getNames name
    vars <- mapM (assignVar srcspan) names
    let cs = map (eq t) vars
    mapM_ logShow cs
  matchTerm t (PInfixApp _ p1 op p2) = return ()
  matchTerm t (PApp _ _ ps) = return ()
  matchTerm t (PTuple _ _ ps) = return ()
  matchTerm t (PList _ ps) = return ()
  matchTerm t (PParen _ p) = return ()
  matchTerm t (PAsPat _ name p) = return ()
  matchTerm t PLit {} = return ()
  matchTerm t PWildCard {} = return ()
  matchTerm t node = error $ "Node type not supported: " ++ show node

instance HasTyping Rhs where
  matchTerm t (UnGuardedRhs srcspan exp) = matchTerm t exp
  matchTerm t node = error ("Node type not support: " ++ show node)

instance HasTyping Exp where
  matchTerm term (InfixApp srcspan e1 op e2) = do
    return ()
  matchTerm term (App srcspan e1 e2) = do
    return ()
  matchTerm term (NegApp srcspan e) = do
    return ()
  matchTerm term (Lambda srcspan pats e) = do
    return ()
  matchTerm term (Let srcspan bind e) = do
    return ()
  matchTerm term (If srcspan e1 e2 e3) = do
    return ()
  matchTerm term (Case srcspan exp alts) = do
    return ()
  matchTerm term (Tuple srcspan _ exps) = do
    return ()
  matchTerm term (List srcspan exps) = do
    return ()
  matchTerm term (Paren srcspan e) = do
    return ()
  matchTerm term (LeftSection srcspan e _) = do
    return ()
  matchTerm term (RightSection srcspan _ e) = do
    return ()
  matchTerm term (Lit srcspan l) = matchTerm term l

  matchTerm term (Var _ (Qual _ (ModuleName _ mname) varname)) = do
    v <- assignQualVar mname (getSingleName varname)
    logShow (eq term v)

  matchTerm term (Var _ (UnQual srcspan varname)) = do
    v <- assignVar srcspan (getSingleName varname)
    logShow (eq term v)

  matchTerm term node = do
    return ()

instance HasTyping Match where
  matchTerm t (Match srcspan name pats rhs maybeWheres) = do
    return ()
  matchTerm t (InfixMatch srcInfo pat name pats rhs maybeWheres) =
    matchTerm t (Match srcInfo name (pat : pats) rhs maybeWheres)

instance HasTyping Literal where
  matchTerm term (Char srcspan _ _) = do
    let c = eq term (atomFrom "char")
    logShow c

  matchTerm term (String srcspan _ _) = do
    let c = eq term (atomFrom "string")
    logShow c

  matchTerm term (Int srcspan _ _) = do
    let c = eq term (atomFrom "int")
    logShow c

  matchTerm term (Frac srcspan _ _) = do
    let c = eq term (atomFrom "float")
    logShow c

  matchTerm _ node = error ("Node type not support: " ++ show node)

constraintsFromCurrentModule :: (HasLogFunc env, HasAST env, HasTypeVarCounter env, HasSlices env) => RIO env ()
constraintsFromCurrentModule = do
  logDebug "Phase: Load constraints"
  mbAst <- readIORefFromLens astL
  case mbAst of
    Nothing -> error "Load constraints started before ast is ready"
    Just ast -> do
      matchTerm unit ast

