{-# LANGUAGE NoImplicitPrelude #-}

module Range where
import RIO
import Language.Haskell.Exts.SrcLoc

data Range = Global | Scoped SrcSpan deriving (Show)

sc :: SrcSpanInfo -> Range
sc = Scoped . srcInfoSpan

global :: Range
global = Global


sp :: SrcSpanInfo -> SrcSpan
sp = srcInfoSpan

isWithin :: Range -> Range -> Bool
isWithin _ Global = True
isWithin Global _ = False
isWithin (Scoped (SrcSpan f1 startl1 startc1 endl1 endc1)) (Scoped (SrcSpan f2 startl2 startc2 endl2 endc2)) =
     let sameFile = f1 == f2
         startAfter
          | startl1 > startl2 = True
          | startl1 == startl2 && startc1 >= startc2 = True
          | otherwise = False
         endBefore
          | endl1 < endl2 = True
          | endl1 == endl2 && endc1 <= endc2 = True
          | otherwise = False
     in sameFile && startAfter && endBefore

