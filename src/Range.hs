{-# LANGUAGE NoImplicitPrelude #-}

module Range where
import RIO
import Language.Haskell.Exts.SrcLoc

data Range = ModVisible Text | GlobalVisible | Scoped SrcSpan deriving (Show)

sc :: SrcSpanInfo -> Range
sc = Scoped . srcInfoSpan

global :: Range
global = GlobalVisible

modVis :: Text -> Range
modVis = ModVisible

sp :: SrcSpanInfo -> SrcSpan
sp = srcInfoSpan

