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
  makeSlices p (PatBind srcspan pat rhs maybeWheres) = do
    addSlices p (getNames pat)
    makeSlices (sc srcspan) rhs
    mapM_ (makeSlices (sc srcspan)) maybeWheres
  makeSlices p (FunBind srcspan matches) = do
    mapM_ (makeSlices p) matches
  makeSlices p node = error ("Node type not support: " ++ show node)

instance HasSlice Match where
  makeSlices p (Match srcspan name pats rhs maybeWheres) = do
    addSlices p (getNames name)
    addSlices (sc srcspan) (getNames pats)
    makeSlices (sc srcspan) rhs
    mapM_ (makeSlices (sc srcspan)) maybeWheres
  makeSlices p (InfixMatch srcInfo pat name pats rhs maybeWheres) =
    makeSlices p (Match srcInfo name (pat : pats) rhs maybeWheres)

instance HasSlice Rhs where
  makeSlices p (UnGuardedRhs srcspan exp) = makeSlices (sc srcspan) exp
  makeSlices p node = error ("Node type not support: " ++ show node)

instance HasSlice Exp where
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
  makeSlices p node = error $ "Unsupported node type: " ++ show node

instance HasSlice Binds where
  makeSlices p (BDecls _ decls) = mapM_ (makeSlices p) decls
  makeSlices p node = error ("Node type not support: " ++ show node)

instance HasSlice Alt where
  makeSlices p (Alt srcspan pat rhs maybeWheres) = do
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
