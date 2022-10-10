{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Mod where
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import RIO
import RIO.Text


moduleName :: Module SrcSpanInfo -> Text
moduleName (Module _ (Just (ModuleHead _ (ModuleName _ name) _ _)) _ _ _) = pack name
moduleName (Module _ Nothing _ _ _) = "Main"
moduleName _ = error "Not a module"
