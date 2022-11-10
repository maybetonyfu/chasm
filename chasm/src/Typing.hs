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
      return $ "typeof_" ++ normModName vMod ++ "_" ++ v ++ "_" ++ vId

assignQualVar :: (HasSlices env) => String -> String -> RIO env String
assignQualVar m v = do
  slices <- readIORefFromLens slicesL
  let mbslice = findSliceByModname slices (T.pack v) (T.pack m)
  case mbslice of
    Nothing -> error ("Cannot find symbol: " ++ v)
    Just slice -> do
      let vId = show (slId slice)
      let vMod = T.unpack (slModuleName slice)
      return $ "typeof_" ++ normModName vMod ++ "_" ++ v ++ "_" ++ vId

addCstrWithId :: (HasConstraints env, HasConstraintCounter env) => Maybe Int -> String -> Term -> SrcSpanInfo -> RIO env ()
addCstrWithId Nothing head body loc = addCstr head body loc
addCstrWithId (Just n) head body loc = do
  constraintsHandle <- view constraintsL
  let constraint = Constraint n head body (srcInfoSpan loc)
  modifyIORef constraintsHandle (constraint :)

addCstr :: (HasConstraintCounter env, HasConstraints env) => String -> Term -> SrcSpanInfo -> RIO env ()
addCstr head body loc = do
  constraintCounterHandle <- view constraintCounterL
  constraintsHandle <- view constraintsL
  count <- readIORef constraintCounterHandle
  let constraint = Constraint count head body (srcInfoSpan loc)
  modifyIORef constraintsHandle (constraint :)
  modifyIORef constraintCounterHandle (+ 1)

newCstrId :: (HasConstraintCounter env) => RIO env Int
newCstrId = do
  constraintCounterHandle <- view constraintCounterL
  newId <- readIORef constraintCounterHandle
  modifyIORef constraintCounterHandle (+ 1)
  return newId

class HasTyping f where
  matchTerm ::
    ( HasLogFunc env,
      HasTypeVarCounter env,
      HasSlices env,
      HasConstraintCounter env,
      HasConstraints env
    ) =>
    (String, Term, Int) ->
    f SrcSpanInfo ->
    RIO env ()

instance HasTyping Module where
  matchTerm (h, t, n) (Module _ _ _ _ decls) = mapM_ (matchTerm (h,t, n)) decls
  matchTerm _ _ = error "Not a module"

instance HasTyping Decl where
  matchTerm (_, t, n) (PatBind srcspan (PVar _ name) rhs maybeWheres) = do
    h <- assignVar srcspan (getSingleName name)
    matchTerm (h, varHead, n) rhs

  matchTerm (_, t, n) (TypeSig srcspan names htype) = do
    let typeTerm = typeToTerm htype
    mapM_
      ( \name -> do
          h <- assignVar srcspan (getSingleName name)
          cstId <- newCstrId
          addCstrWithId (Just cstId) h (eq varHead typeTerm) (ann htype)
      )
      names
  matchTerm (_, t, n) (FunBind srcspan matches@(match : _)) = do
    let (Match _ name _ _ _) = match
    h <- assignVar srcspan (getSingleName name)
    mapM_ (matchTerm (h, varHead, n)) matches
  matchTerm _ node = error ("Node type not support: " ++ show node)

instance HasTyping Pat where
  -- matchTerm (h, t) (PVar srcspan name) = retrun ()
  -- matchTerm t (PIgnfixApp _ p1 op p2) = return ()
  -- matchTerm t (PApp _ _ pats) = return ()
  -- matchTerm t (PTuple _ _ ps) = return ()
  -- matchTerm t (PList _ ps) = return ()
  -- matchTerm t (PAsPat _ name p) = return ()
  matchTerm (h, t, n) (PApp srcspan (UnQual _ (Ident _ "True")) []) = do
    newId <- newCstrId
    addCstrWithId (Just newId) h (eq t (atomFrom "haskell_Bool")) srcspan
  matchTerm (h, t, n) (PApp srcspan (UnQual _ (Ident _ "False")) []) = do
    newId <- newCstrId
    addCstrWithId (Just newId) h (eq t (atomFrom "haskell_Bool")) srcspan
  matchTerm (h, t, n) (PParen _ p) = matchTerm (h, t, n) p
  matchTerm (h, t, n) (PLit _ _ l) = matchTerm (h, t, n) l
  matchTerm _ PWildCard {} = return ()
  matchTerm _ node = error $ "Node type not supported: " ++ show node

instance HasTyping Rhs where
  matchTerm (h, t, n) (UnGuardedRhs srcspan exp) = matchTerm (h, t, n) exp
  matchTerm _ node = error ("Node type not support: " ++ show node)

instance HasTyping Exp where
  -- matchTerm term (InfixApp srcspan e1 op e2) = return ()
  -- matchTerm term (App srcspan e1 e2) =  return ()
  -- matchTerm term (NegApp srcspan e) =  return ()
  -- matchTerm term (Lambda srcspan pats e) =  return ()
  -- matchTerm term (Let srcspan bind e) =  return ()
  -- matchTerm term (If srcspan e1 e2 e3) =  return ()
  -- matchTerm term (Case srcspan exp alts) = return ()
  -- matchTerm term (Tuple srcspan _ exps) =  return ()
  -- matchTerm term (List srcspan exps) =  return ()
  -- matchTerm term (LeftSection srcspan e _) =  return ()
  -- matchTerm term (RightSection srcspan _ e) =  return ()
  matchTerm (h, t, n) (Paren srcspan e) = matchTerm (h, t, n) e
  matchTerm (h, t, n) (Con srcspan (UnQual _ (Ident _ "True"))) = do
    newId <- newCstrId
    addCstrWithId (Just newId) h (eq t (atomFrom "haskell_Bool")) srcspan
  matchTerm (h, t, n) (Con srcspan (UnQual _ (Ident _ "False"))) = do
    newId <- newCstrId
    addCstrWithId (Just newId) h (eq t (atomFrom "haskell_Bool")) srcspan
  matchTerm (h, t, n) (Lit srcspan l) = matchTerm (h, t, n) l
  matchTerm (h, t, n) (Var srcspan (Qual _ (ModuleName _ mname) varname)) = do
    vname <- assignQualVar mname (getSingleName varname)
    newId <- newCstrId
    addCstrWithId (Just newId) h (typeOf vname t) srcspan
  matchTerm (h, t, n) (Var _ (UnQual srcspan varname)) = do
    vname <- assignVar srcspan (getSingleName varname)
    newId <- newCstrId
    addCstrWithId (Just newId) h (typeOf vname t) srcspan
  matchTerm _ node = error ("Node type not support: " ++ show node)

instance HasTyping Match where
  matchTerm (h, t, n) (Match srcspan name pats rhs maybeWheres) = do
    let matchArg pat = do
          v <- fresh
          matchTerm (h, v, n) pat
          return v
    vargs <- mapM matchArg pats
    vrhs <- fresh
    matchTerm (h, vrhs, n) rhs
    newId <- newCstrId
    addCstrWithId (Just newId) h (eq t (functionFrom (vargs ++ [vrhs]))) (ann name)

  matchTerm htn (InfixMatch srcInfo pat name pats rhs maybeWheres) =
    matchTerm htn (Match srcInfo name (pat : pats) rhs maybeWheres)

instance HasTyping Literal where
  matchTerm (h, t, n) (Char srcspan _ _) = do
    newId <- newCstrId
    addCstrWithId (Just newId) h (eq t (atomFrom "haskell_Char")) srcspan
  matchTerm (h, t, n) (String srcspan _ _) = do
    newId <- newCstrId
    addCstrWithId (Just newId) h (eq t (atomFrom "haskell_String")) srcspan
  matchTerm (h, t, n) (Int srcspan _ _) = do
    newId <- newCstrId
    addCstrWithId (Just newId) h (eq t (atomFrom "haskell_Int")) srcspan
  matchTerm (h, t, n) (Frac srcspan _ _) = do
    newId <- newCstrId
    addCstrWithId (Just newId) h (eq t (atomFrom "haskell_Float")) srcspan
  matchTerm _ node = error ("Node type not support: " ++ show node)

constraintsFromCurrentModule ::
  ( HasLogFunc env,
    HasAST env,
    HasTypeVarCounter env,
    HasSlices env,
    HasConstraintCounter env,
    HasConstraints env
  ) =>
  RIO env ()
constraintsFromCurrentModule = do
  logDebug "Phase: Load constraints"
  mbAst <- readIORefFromLens astL
  case mbAst of
    Nothing -> error "Load constraints started before ast is ready"
    Just ast -> do
      matchTerm ("", unit, -1) ast

constraintsFromBottles ::
  ( HasLogFunc env,
    HasTypeVarCounter env,
    HasBottle env,
    HasSlices env,
    HasConstraintCounter env,
    HasConstraints env,
    HasLoad env
  ) =>
  RIO env ()
constraintsFromBottles = do
  logDebug "Phase: Load constraints from bottles"
  bottles <- readIORefFromLens bottleL
  loads <- readIORefFromLens loadL
  mapM_ (constraintFromBottle loads) bottles

constraintFromBottle ::
  ( HasLogFunc env,
    HasTypeVarCounter env,
    HasSlices env,
    HasConstraintCounter env,
    HasConstraints env
  ) =>
  [Load] ->
  Bottle ->
  RIO env ()
constraintFromBottle loads (Bottle mname mpath drops)  = do
  mapM_ (constraintFromDrop loads mname ) drops

constraintFromDrop ::
  ( HasLogFunc env,
    HasTypeVarCounter env,
    HasSlices env,
    HasConstraintCounter env,
    HasConstraints env
  ) =>
  [Load] ->
  Text ->
  (Text, Type SrcSpanInfo) ->
  RIO env ()
constraintFromDrop loads mname (vname, vtype) = do
  let term = typeToTerm vtype
  let mbloc = findLoadLocation loads mname vname
  case mbloc of
    Nothing -> return ()
    Just loc -> do
     v <- assignQualVar (T.unpack mname) (T.unpack vname)
     newId <- newCstrId
     addCstrWithId (Just newId) v (eq varHead term) loc

findLoadLocation :: [Load] -> Text -> Text -> Maybe SrcSpanInfo
findLoadLocation loads mname vname =
  let go (Load name loc _ _ Everything) = name == mname
      go (Load name loc _ _ (Inc vars)) = name == mname && vname `elem` vars
      go (Load name loc _ _ (Exc vars)) = name == mname && vname `notElem` vars
  in fmap loadLoc (find go loads)

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
