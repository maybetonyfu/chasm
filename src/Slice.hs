{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Slice where

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import Namable
import RIO
import RIO.List

data Range = Global | Scoped SrcSpan deriving (Show)

data Slices = Slices
  { logFun :: LogFunc,
    getSlices :: IORef [(Range, [String])]
  }

instance HasLogFunc Slices where
  logFuncL :: Lens' Slices LogFunc
  logFuncL = lens logFun (\x y -> x {logFun = y})

sc :: SrcSpanInfo -> Range
sc = Scoped . srcInfoSpan

global :: Range
global = Global

class HasSlice f where
  makeSlices :: Range -> f SrcSpanInfo -> RIO Slices ()

addSlices :: Range -> [String] -> RIO Slices ()
addSlices range names = do
  s <- ask
  modifyIORef (getSlices s) (++ [(range, names)])

instance HasSlice Module where
  makeSlices _ (Module _ _ _ _ decls) = do
    logInfo "Module"
    mapM_ (makeSlices global) decls
  makeSlices _ _ = error "Not a module"

instance HasSlice Decl where
  makeSlices parent (PatBind srcspan pat rhs maybeWheres) = do
    addSlices parent (getNames pat)
    makeSlices (sc srcspan) rhs
    mapM_ (makeSlices (sc srcspan)) maybeWheres
  makeSlices parent (FunBind srcspan matches) = do
    mapM_ (makeSlices parent) matches
  makeSlices _ p = error ("Node type not support: " ++ show p)

instance HasSlice Match where
  makeSlices parent (Match srcspan name pats rhs maybeWheres) = do
    addSlices parent (getNames name)
    addSlices (sc srcspan) (getNames pats)
    makeSlices (sc srcspan) rhs
    mapM_ (makeSlices (sc srcspan)) maybeWheres
  makeSlices parent (InfixMatch srcInfo pat name pats rhs maybeWheres) =
    makeSlices parent (Match srcInfo name (pat : pats) rhs maybeWheres)

instance HasSlice Rhs where
  makeSlices _ (UnGuardedRhs srcspan exp) = makeSlices (sc srcspan) exp
  makeSlices _ p = error ("Node type not support: " ++ show p)

instance HasSlice Exp where
  makeSlices _ (InfixApp srcspan e1 _ e2) = mapM_ (makeSlices (sc srcspan)) [e1, e2]
  makeSlices _ (App srcspan e1 e2) = mapM_ (makeSlices (sc srcspan)) [e1, e2]
  makeSlices _ (NegApp srcspan e) = makeSlices (sc srcspan) e
  makeSlices _ (Lambda srcspan pats e) = addSlices (sc srcspan) (getNames pats) >> makeSlices (sc srcspan) e
  makeSlices _ (Let srcspan bind e) = makeSlices (sc srcspan) bind >> makeSlices (sc srcspan) e
  makeSlices _ (If srcspan e1 e2 e3) = mapM_ (makeSlices (sc srcspan)) [e1, e2, e3]
  makeSlices _ (Case srcspan exp alts) = makeSlices (sc srcspan) exp >> mapM_ (makeSlices (sc srcspan)) alts
  makeSlices _ (Tuple srcspan _ exps) = mapM_ (makeSlices (sc srcspan)) exps
  makeSlices _ (List srcspan exps) = mapM_ (makeSlices (sc srcspan)) exps
  makeSlices _ (Paren srcspan e) = makeSlices (sc srcspan) e
  makeSlices _ (LeftSection srcspan e _) = makeSlices (sc srcspan) e
  makeSlices _ (RightSection srcspan _ e) = makeSlices (sc srcspan) e
  makeSlices _ node = error $ "Unsupported node type: " ++ show node

instance HasSlice Binds where
  makeSlices parent (BDecls _ decls) = mapM_ (makeSlices parent) decls
  makeSlices _ p = error ("Node type not support: " ++ show p)

instance HasSlice Alt where
  makeSlices parent (Alt srcspan pat rhs maybeWheres) = do
    addSlices (sc srcspan) (getNames pat)
    makeSlices (sc srcspan) rhs
    mapM_ (makeSlices (sc srcspan)) maybeWheres

main :: IO ()
main = runSimpleApp $ do
  logFunc <- view logFuncL
  let contents = "x = 1"
  let pResult = parseModuleWithMode parseMode contents
      parseMode = defaultParseMode {parseFilename = "MyFile"}
  case pResult of
    ParseOk hModule -> do
      logInfo "OK"
      emptyList <- newIORef []
      x <- runRIO (Slices logFunc emptyList) (makeSlices global hModule >> ask)
      s <- readIORef (getSlices x)
      mapM_
        ( \(loc, symbols) -> do
            logInfo (displayShow symbols)
            logInfo (displayShow loc)
        )
        s
    ParseFailed srcLoc message ->
      logInfo "Parsing Failed"
