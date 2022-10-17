{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Slice where

import Bottle
import Data.String.QQ
import Language.Haskell.Exts
import Lenses
import Namable
import RIO
import RIO.List
import qualified RIO.Text as T
import Range
import Types

class Sliceable f where
  makeSlices :: (HasSlices env, HasSliceCounter env, HasLogFunc env, HasTargetName env) => Range -> f SrcSpanInfo -> RIO env ()

addSlices :: (HasSlices env, HasSliceCounter env, HasLogFunc env, HasTargetName env) => Range -> [String] -> RIO env ()
addSlices range names = do
  counterHandle <- view sliceCounterL
  slicesHandle <- view slicesL
  counter <- readIORef counterHandle
  targetNameHandle <- view targetNameL
  targetName <- readIORef targetNameHandle
  let slice =
        Slice
          { slRange = range,
            slSymbols = fmap T.pack names,
            slModuleName = targetName,
            slId = counter
          }
  modifyIORef counterHandle (+ 1)
  modifyIORef slicesHandle (slice :)

instance Sliceable Module where
  makeSlices _ (Module _ _ _ _ decls) = do
    logInfo "Module"
    mapM_ (makeSlices global) decls
  makeSlices _ _ = error "Not a module"

instance Sliceable Decl where
  makeSlices p (PatBind srcspan pat rhs maybeWheres) = do
    addSlices p (getNames pat)
    makeSlices (sc srcspan) rhs
    mapM_ (makeSlices (sc srcspan)) maybeWheres
  makeSlices p (FunBind srcspan matches) = do
    mapM_ (makeSlices p) matches
  makeSlices p node = error ("Node type not support: " ++ show node)

instance Sliceable Match where
  makeSlices p (Match srcspan name pats rhs maybeWheres) = do
    addSlices p (getNames name)
    addSlices (sc srcspan) (getNames pats)
    makeSlices (sc srcspan) rhs
    mapM_ (makeSlices (sc srcspan)) maybeWheres
  makeSlices p (InfixMatch srcInfo pat name pats rhs maybeWheres) =
    makeSlices p (Match srcInfo name (pat : pats) rhs maybeWheres)

instance Sliceable Rhs where
  makeSlices p (UnGuardedRhs srcspan exp) = makeSlices (sc srcspan) exp
  makeSlices p node = error ("Node type not support: " ++ show node)

instance Sliceable Exp where
  makeSlices p (InfixApp srcspan e1 _ e2) = mapM_ (makeSlices (sc srcspan)) [e1, e2]
  makeSlices p (App srcspan e1 e2) = mapM_ (makeSlices (sc srcspan)) [e1, e2]
  makeSlices p (NegApp srcspan e) = makeSlices (sc srcspan) e
  makeSlices p (Lambda srcspan pats e) = addSlices (sc srcspan) (getNames pats) >> makeSlices (sc srcspan) e
  makeSlices p (Let srcspan bind e) = makeSlices (sc srcspan) bind >> makeSlices (sc srcspan) e
  makeSlices p (If srcspan e1 e2 e3) = mapM_ (makeSlices (sc srcspan)) [e1, e2, e3]
  makeSlices p (Case srcspan exp alts) = makeSlices (sc srcspan) exp >> mapM_ (makeSlices (sc srcspan)) alts
  makeSlices p (Tuple srcspan _ exps) = mapM_ (makeSlices (sc srcspan)) exps
  makeSlices p (List srcspan exps) = mapM_ (makeSlices (sc srcspan)) exps
  makeSlices p (Paren srcspan e) = makeSlices (sc srcspan) e
  makeSlices p (LeftSection srcspan e _) = makeSlices (sc srcspan) e
  makeSlices p (RightSection srcspan _ e) = makeSlices (sc srcspan) e
  makeSlices p (Lit _ _) = return ()
  makeSlices p (Var _ qname) = return ()
  makeSlices p (Con _ _) = return ()
  makeSlices p node = error $ "Unsupported node type: " ++ show node

instance Sliceable Binds where
  makeSlices p (BDecls _ decls) = mapM_ (makeSlices p) decls
  makeSlices p node = error ("Node type not support: " ++ show node)

instance Sliceable Alt where
  makeSlices p (Alt srcspan pat rhs maybeWheres) = do
    addSlices (sc srcspan) (getNames pat)
    makeSlices (sc srcspan) rhs
    mapM_ (makeSlices (sc srcspan)) maybeWheres

loadSlicesCurrentModule :: (HasLogFunc env, HasAST env, HasSlices env, HasSliceCounter env, HasTargetName env) => RIO env ()
loadSlicesCurrentModule = do
  logDebug "Phase: Load slices from current module"
  astHandle <- view astL
  maybeAST <- readIORef astHandle
  case maybeAST of
    Nothing -> error "Generating slices from current module with empty AST"
    Just hmodule -> do
      makeSlices global hmodule

bottleToSlice :: (HasSliceCounter env, HasSlices env, HasTargetName env, HasLoad env) => Bottle -> RIO env ()
bottleToSlice (Bottle name _ drops) = do
  counterHandle <- view sliceCounterL
  slicesHandle <- view slicesL
  counter <- readIORef counterHandle
  let slice =
        Slice
          { slRange = global,
            slSymbols = fmap fst drops,
            slModuleName = name,
            slId = counter
          }
  modifyIORef counterHandle (+ 1)
  modifyIORef slicesHandle (slice :)

optimizeBottleDrops :: [Load] -> Bottle -> Bottle
optimizeBottleDrops loads b@(Bottle name path drops) =
  let matchingLoad = find ((== name) . loadName) loads
   in case matchingLoad of
        Nothing -> b
        Just l ->
          case loadVars l of
            All -> b
            Inc vs -> Bottle name path (filter ((`elem` vs) . fst) drops)
            Exc vs -> Bottle name path (filter ((`notElem` vs) . fst) drops)

loadSlicesFromBottles :: (HasLogFunc env, HasBottle env, HasSlices env, HasSliceCounter env, HasTargetName env, HasLoad env) => RIO env ()
loadSlicesFromBottles = do
  logDebug "Phase: Load slices from external modules"
  bottleHandle <- view bottleL
  bottles <- readIORef bottleHandle
  targetNameHandle <- view targetNameL
  targetName <- readIORef targetNameHandle
  loadHandle <- view loadL
  loads <- readIORef loadHandle
  let externalBottles = filter (\b -> bottleName b /= targetName) bottles
  let optimalBottles = map (optimizeBottleDrops loads) externalBottles
  mapM_ bottleToSlice optimalBottles
