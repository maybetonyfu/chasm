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

data Slices = Slices
  { logFun :: LogFunc,
    slices :: IORef [(Maybe SrcSpan, [String])]
  }

instance HasLogFunc Slices where
  logFuncL :: Lens' Slices LogFunc
  logFuncL = lens logFun (\x y -> x {logFun = y})

sp :: SrcSpanInfo -> SrcSpan
sp = srcInfoSpan

class HasSlice f where
  getSlices :: f SrcSpanInfo -> RIO Slices ()

instance HasSlice Module where
  getSlices :: Module SrcSpanInfo -> RIO Slices ()
  getSlices (Module _ _ _ _ decls) = do
    logInfo "Module"
  getSlices _ = error "Not a module"

instance HasSlice Decl where
  getSlices :: Decl SrcSpanInfo -> RIO Slices ()
  getSlices (PatBind srcspan pat rhs maybeWheres) = do
    logInfo "Hello"
    s <- ask
    modifyIORef (slices s) (++ [(Just (sp srcspan), getNames pat)])
    -- return ()

main :: IO ()
main = runSimpleApp $ do
  logFunc <- view logFuncL
  let contents = "module X where"
  let pResult = parseModuleWithMode parseMode contents
      parseMode = defaultParseMode {parseFilename = "MyFile"}
  case pResult of
    ParseOk hModule -> do
      logInfo "OK"
      emptyList <- newIORef []
      x <- runRIO (Slices logFunc emptyList) (getSlices hModule >> ask)
      return ()
    ParseFailed srcLoc message ->
      logInfo "Parsing Failed"
