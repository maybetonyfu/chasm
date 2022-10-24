{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Typing where

import Data.String.QQ
import Goal
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import qualified Language.Prolog
import Lenses
import Namable
import RIO
import RIO.List
import qualified RIO.Text as T
import Range
import Types
import Var

type Term = Language.Prolog.Term

logShow :: (Show a, HasLogFunc env) => a -> RIO env ()
logShow = logInfo . displayShow

fresh :: (HasTypeVarCounter env) => RIO env Term
fresh = do
  typeVarCounterHandle <- view typeVarCounterL
  value <- readIORef typeVarCounterHandle
  modifyIORef typeVarCounterHandle (+ 1)
  return (varFrom ("Fresh_" ++ show value))

assignVar :: (HasSlices env) => SrcSpanInfo -> String -> RIO env String
assignVar srcspan v = do
  slices <- readIORefFromLens slicesL
  let mbslice = findSliceByRange slices (T.pack v) srcspan
  case mbslice of
    Nothing -> error ("Cannot find symbol: " ++ v)
    Just slice -> do
      let vId = show (slId slice)
      let vMod = T.unpack (slModuleName slice)
      return $ normModName vMod ++ "_" ++ v ++ "_" ++ vId

assignQualVar :: (HasSlices env) => String -> String -> RIO env String
assignQualVar m v = do
  slices <- readIORefFromLens slicesL
  let mbslice = findSliceByModname slices (T.pack v) (T.pack m)
  case mbslice of
    Nothing -> error ("Cannot find symbol: " ++ v)
    Just slice -> do
      let vId = show (slId slice)
      let vMod = T.unpack (slModuleName slice)
      return $ normModName vMod ++ "_" ++ v ++ "_" ++ vId

addCstr :: (HasConstraintCounter env, HasConstraints env) => String -> Term -> RIO env ()
addCstr head body = do
  constraintCounterHandle <- view constraintCounterL
  constraintsHandle <- view constraintsL
  count <- readIORef constraintCounterHandle
  let constraint = Constraint count head body
  modifyIORef constraintsHandle (constraint :)
  modifyIORef constraintCounterHandle (+ 1)

class HasTyping f where
  matchTerm ::
    ( HasLogFunc env,
      HasTypeVarCounter env,
      HasSlices env,
      HasConstraintCounter env,
      HasConstraints env
    ) =>
    (String, Term) ->
    f SrcSpanInfo ->
    RIO env ()

instance HasTyping Module where
  matchTerm t (Module _ _ _ _ decls) = mapM_ (matchTerm t) decls
  matchTerm _ _ = undefined

instance HasTyping Decl where
  matchTerm (_, t) (PatBind srcspan (PVar _ name) rhs maybeWheres) = do
    ident <- assignVar srcspan (getSingleName name)
    let h = "typeof_" ++ ident
    matchTerm (h, varHead) rhs
  matchTerm (_, t) (TypeSig srcspan names htype) = do
    let typeTerm = typeToTerm htype
    mapM_
      ( \name -> do
          ident <- assignVar srcspan (getSingleName name)
          addCstr ("typeof_" ++ ident) (eq varHead typeTerm)
      )
      names

  matchTerm ht (FunBind srcspan matches) = do
    mapM_ (matchTerm ht) matches
  matchTerm _ node = error ("Node type not support: " ++ show node)

instance HasTyping Pat where
  -- matchTerm (h, t) (PVar srcspan name) = retrun ()
  -- matchTerm t (PInfixApp _ p1 op p2) = return ()
  -- matchTerm t (PApp _ _ ps) = return ()
  -- matchTerm t (PTuple _ _ ps) = return ()
  -- matchTerm t (PList _ ps) = return ()
  -- matchTerm t (PParen _ p) = return ()
  -- matchTerm t (PAsPat _ name p) = return ()
  -- matchTerm t PLit {} = return ()
  -- matchTerm t PWildCard {} = return ()
  matchTerm t node = error $ "Node type not supported: " ++ show node

instance HasTyping Rhs where
  matchTerm ht (UnGuardedRhs srcspan exp) = matchTerm ht exp
  matchTerm t node = error ("Node type not support: " ++ show node)

instance HasTyping Exp where
  -- matchTerm term (InfixApp srcspan e1 op e2) = do
  --   return ()
  -- matchTerm term (App srcspan e1 e2) = do
  --   return ()
  -- matchTerm term (NegApp srcspan e) = do
  --   return ()
  -- matchTerm term (Lambda srcspan pats e) = do
  --   return ()
  -- matchTerm term (Let srcspan bind e) = do
  --   return ()
  -- matchTerm term (If srcspan e1 e2 e3) = do
  --   return ()
  -- matchTerm term (Case srcspan exp alts) = do
  --   return ()
  -- matchTerm term (Tuple srcspan _ exps) = do
  --   return ()
  -- matchTerm term (List srcspan exps) = do
  --   return ()
  -- matchTerm term (Paren srcspan e) = do
  --   return ()
  -- matchTerm term (LeftSection srcspan e _) = do
  --   return ()
  -- matchTerm term (RightSection srcspan _ e) = do
  --   return ()
  matchTerm (h, t) (Con srcspan (UnQual _ (Ident _ "True"))) = addCstr h (eq t (atomFrom "haskell_Bool"))
  matchTerm (h, t) (Con srcspan (UnQual _ (Ident _ "False"))) = addCstr h (eq t (atomFrom "haskell_Bool"))
  matchTerm ht (Lit srcspan l) = matchTerm ht l
  matchTerm (h, t) (Var _ (Qual _ (ModuleName _ mname) varname)) = do
    vname <- assignQualVar mname (getSingleName varname)
    addCstr h (typeOf vname t)
  matchTerm (h, t) (Var _ (UnQual srcspan varname)) = do
    vname <- assignVar srcspan (getSingleName varname)
    addCstr h (typeOf vname t)
  matchTerm term node = error ("Node type not support: " ++ show node)

instance HasTyping Match where
  matchTerm ht (Match srcspan name pats rhs maybeWheres) = do
    undefined
  matchTerm ht (InfixMatch srcInfo pat name pats rhs maybeWheres) =
    matchTerm ht (Match srcInfo name (pat : pats) rhs maybeWheres)

instance HasTyping Literal where
  matchTerm (h, t) (Char srcspan _ _) = addCstr h (eq t (atomFrom "haskell_Char"))
  matchTerm (h, t) (String srcspan _ _) = addCstr h (eq t (atomFrom "haskell_String"))
  matchTerm (h, t) (Int srcspan _ _) = addCstr h (eq t (atomFrom "haskell_Int"))
  matchTerm (h, t) (Frac srcspan _ _) = addCstr h (eq t (atomFrom "haskell_Float"))
  matchTerm _ node = error ("Node type not support: " ++ show node)

constraintsFromCurrentModule :: (
  HasLogFunc env,
  HasAST env,
  HasTypeVarCounter env,
  HasSlices env,
  HasConstraintCounter env,
  HasConstraints env
  ) => RIO env ()
constraintsFromCurrentModule = do
  logDebug "Phase: Load constraints"
  mbAst <- readIORefFromLens astL
  case mbAst of
    Nothing -> error "Load constraints started before ast is ready"
    Just ast -> do
      matchTerm ("", unit) ast

constraintsFromBottles ::
  ( HasLogFunc env,
    HasTypeVarCounter env,
    HasBottle env,
    HasSlices env,
    HasConstraintCounter env,
    HasConstraints env
  ) =>
  RIO env ()
constraintsFromBottles = do
  logDebug "Phase: Load constraints from bottles"
  bottles <- readIORefFromLens bottleL
  mapM_ constraintFromBottle bottles

constraintFromBottle ::
  ( HasLogFunc env,
    HasTypeVarCounter env,
    HasSlices env,
    HasConstraintCounter env,
    HasConstraints env
  ) =>
  Bottle ->
  RIO env ()
constraintFromBottle (Bottle mname mpath drops) = do
  mapM_ (constraintFromDrop mname) drops

constraintFromDrop ::
  ( HasLogFunc env,
    HasTypeVarCounter env,
    HasSlices env,
    HasConstraintCounter env,
    HasConstraints env
  ) =>
  Text ->
  (Text, Type SrcSpanInfo) ->
  RIO env ()
constraintFromDrop mname (vname, vtype) = do
  let term = typeToTerm vtype
  v <- assignQualVar (T.unpack mname) (T.unpack vname)
  addCstr (mconcat ["typeof_", v]) (eq varHead term)

typeToTerm :: Type SrcSpanInfo -> Term
typeToTerm (TyVar _ name) = varFrom ("TVar_" ++ getSingleName name)
typeToTerm (TyCon _ (Qual _ mname name)) = atomFrom ("tc_" ++ normModName (getSingleName mname) ++ "_" ++ getSingleName name)
typeToTerm (TyCon _ (UnQual _ (Ident _ x))) = atomFrom ("haskell_" ++ x)
-- typeToTerm  (TyList _ t) = lstOf (typeToTerm bindings t)
-- typeToTerm  (TyFun _ t1 t2) = funOf [typeToTerm bindings t1, typeToTerm bindings t2]
-- typeToTerm  (TyTuple _ _ ts) = tupOf . map (typeToTerm bindings) $ ts
-- typeToTerm  (TyUnboxedSum _ ts) = tupOf . map (typeToTerm bindings) $ ts
-- typeToTerm  (TyApp _ t1 t2) = Pair (typeToTerm bindings t1) (typeToTerm bindings t2) []
-- typeToTerm  (TyParen _ t) = typeToTerm bindings t
typeToTerm t = error ("Unsupported type: " ++ show t)

normModName :: String -> String
normModName [] = []
normModName ('.' : xs) = normModName xs
normModName (x : xs) = x : normModName xs
