module Slice where
import RIO
import RIO.State
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax


data ScopeType
  = VarScope
  | FunScope
  | ArgScope
  | LambdaScope
  | PatternScope
  | CaseScope
  | LetScope
  | GeneratorScope
  | AdtScope
  | RecordFieldScope
  | TypeScope
  | OperatorScope
  | TypeSigScope
  deriving (Show, Eq)

data Range = Normal SrcSpan | Global deriving (Show, Eq)

data Slice = Slice {
  moduleName :: String,
  moduleAlias :: String,
  sliceId :: Int,
  scope :: Range,
  scopeType:: ScopeType
}

getId :: HasLogFunc m => RIO m (State Int Int)
getId = do return (do 
    n <- get
    return n)
  


sp :: SrcSpanInfo -> SrcSpan
sp = srcInfoSpan

class HasSlices f where
  getSlices :: f SrcSpanInfo -> StateT Int IO [Slice]

instance HasSlices Module where
  getSlices  (Module _ _ _ _ decls) = return []
  getSlices _ = error "Not a module"

-- instance HasSlices Decl where
--   getSlices (PatBind srcspan pat rhs maybeWheres) = do
--     let scope = parent
--         names = patternVarNames pat
--         bindings = setMapFromList (Binding parent VarScope) names
--         rhsBindings = getSlices (Normal . sp $ srcspan) rhs
--         wherebinds = case maybeWheres of
--           Nothing -> []
--           Just wheres -> getSlices (Normal . sp $ srcspan) wheres
--     return bindings ++ wherebinds ++ rhsBindings
