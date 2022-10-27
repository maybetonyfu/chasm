{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Load where

import RIO
import Types
import Lenses
import RIO.Text as T
import Language.Haskell.Exts

class HasImport f where
  registerImports :: (HasLoad env, HasLogFunc env, HasTargetName env) => f SrcSpanInfo -> RIO env ()

packModName (ModuleName _ name) = T.pack name

instance HasImport Module where
  registerImports (Module _ Nothing  _ impts _) = mapM_ registerImports impts
  registerImports (Module _ (Just (ModuleHead _ modName _ _))  _ impts _) = do
    targetNameHandle <- view targetNameL
    let mName = packModName modName
    writeIORef targetNameHandle mName
    mapM_ registerImports impts
  registerImports _ = error "Not a module"

instance HasImport ImportDecl where
  registerImports (ImportDecl srcspan modName isQual _ _ _ maybeAs maybeSpecs) = do
    let name' = packModName modName
    let as' = fmap packModName maybeAs
    let packName (Ident _ n) = T.pack n
        packName (Symbol _ n) = T.pack n
    let extractImportList [] = []
        extractImportList (IVar _ name:ss) = packName name : extractImportList ss
        extractImportList (_:ss) = extractImportList ss
    let items' = case maybeSpecs of
          Nothing -> Everything
          Just (ImportSpecList _ False specs) -> Inc (extractImportList specs)
          Just (ImportSpecList _ True specs) -> Exc (extractImportList specs)
    let load' = Load
         { loadName = name'
         , loadLoc = srcspan
         , loadQual = isQual
         , loadAs = as'
         , loadVars = items'
         }
    loadHandle <- view loadL
    modifyIORef loadHandle (load':)

analyzeImports :: (HasLoad env, HasLogFunc env, HasAST env, HasTargetName env) => RIO env ()
analyzeImports = do
  logDebug "Stage: Analyzing imports"
  astHandle <- view astL
  maybeAST <- readIORef astHandle
  case maybeAST of
    Nothing -> error "NO AST during analyzing import phase"
    Just hmodule -> registerImports hmodule

