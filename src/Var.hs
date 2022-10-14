{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Var where

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import Mod
import RIO
import Range
import Slice
import RIO.List (find)

type ModName = Text
type SymName = String

findSliceByRange :: [Slice] -> SymName -> SrcSpanInfo -> Maybe Slice
findSliceByRange slices sym srcspan =
  let containsSymbol = filter (\s -> sym `elem` getSymbols s) slices
      go slice maybeSlice =
        case maybeSlice of
          Nothing -> Just slice
          Just slice' -> if getRange slice `isWithin` getRange slice' then Just slice else Just slice'
   in foldr go Nothing containsSymbol

findSliceByModname :: [Slice] -> SymName -> ModName -> Maybe Slice
findSliceByModname slices sym modname =
  let matchingModule slice = getModuleName slice == modname
      matchingSlices = filter matchingModule slices
      containSymbol slice = sym `elem` getSymbols slice
  in find containSymbol matchingSlices

main :: IO ()
main = runSimpleApp $ do
  logFunc <- view logFuncL
  let contents = testSample
  let pResult = parseModuleWithMode parseMode contents
      parseMode = defaultParseMode {parseFilename = "MyFile"}
  case pResult of
    ParseOk hModule -> do
      logInfo "OK"
      emptyList <- newIORef []
      sliceObj <- runRIO (SliceAssemble logFunc emptyList) (makeSlices global hModule >> ask)
      slices' <- readIORef (getSlices sliceObj)
      let modName = moduleName hModule
      let slices = _genSlices modName slices'
      return ()
    ParseFailed srcLoc message ->
      logInfo "Parsing Failed"
