{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Var where

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import RIO
import Range
import Types
import RIO.List (find)

type ModName = Text
type SymName = Text

findSliceByRange :: [Slice] -> SymName -> SrcSpanInfo -> Maybe Slice
findSliceByRange slices sym srcspan =
  let containsSymbol = filter (\s -> sym `elem` slSymbols s) slices
      go slice maybeSlice =
        case maybeSlice of
          Nothing -> Just slice
          Just slice' -> if slRange slice `isWithin` slRange slice' then Just slice else Just slice'
   in foldr go Nothing containsSymbol

findSliceByModname :: [Slice] -> SymName -> ModName -> Maybe Slice
findSliceByModname slices sym modname =
  let matchingModule slice = slModuleName slice == modname
      matchingSlices = filter matchingModule slices
      containSymbol slice = sym `elem` slSymbols slice
  in find containSymbol matchingSlices

