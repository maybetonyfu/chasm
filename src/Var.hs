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

minimalSliceId :: [Slice] -> String -> SrcSpanInfo -> Maybe Int
minimalSliceId slices sym srcspan =
  let containsSymbol = filter (\s -> sym `elem` getSymbols s) slices
      go slice maybeSlice =
        case maybeSlice of
          Nothing -> Just slice
          Just slice' -> if getRange slice `isWithin` getRange slice' then Just slice else Just slice'
      minimalSlice = foldr go Nothing containsSymbol
   in fmap getSliceId minimalSlice

minimalSlice :: [Slice] -> String -> SrcSpanInfo -> Maybe Slice
minimalSlice slices sym srcspan =
  let containsSymbol = filter (\s -> sym `elem` getSymbols s) slices
      go slice maybeSlice =
        case maybeSlice of
          Nothing -> Just slice
          Just slice' -> if getRange slice `isWithin` getRange slice' then Just slice else Just slice'
   in foldr go Nothing containsSymbol

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
      let slices = genSlices modName slices'
      return ()
    ParseFailed srcLoc message ->
      logInfo "Parsing Failed"
