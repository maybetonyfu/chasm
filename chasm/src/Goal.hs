{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Goal  where
import RIO
import qualified RIO.List as L
import Types
import Lenses
import Language.Prolog
import qualified Text.Parsec as P
import qualified RIO.Text as T
import qualified RIO.Text.Partial as TP

unit = Struct "unit" []
varHead = varFrom "T"

atomFrom :: String -> Term
atomFrom x = Struct x []

varFrom = var

typeOf :: String -> Term -> Term
typeOf x t = Struct x [t]

-- funFrom :: Term -> [Term] -> Term
-- funFrom  x [] = unit
-- funFrom fname (x:xs) = Struct "function" [fname, x, funFrom Wildcard xs]
functionFrom :: [Term] -> Term
functionFrom [] = unit
functionFrom (x:xs) = Struct "function" [x, functionFrom xs]

structFunctor :: Term -> Maybe Atom
structFunctor (Struct x _) = Just x
structFunctor _ = Nothing

structArgs :: Term -> Maybe [Term]
structArgs (Struct _ x) = Just x
structArgs _ = Nothing

isStruct :: Term -> Bool
isStruct (Struct _ _) = True
isStruct _ = False

isVar :: Term -> Bool
isVar (Var _) = True
isVar _ = False

eq :: Term -> Term -> Term
eq a b = Struct "=" [a, b]

wellTyped :: (HasConstraints env, HasLogFunc env) => RIO env Bool
wellTyped = do
  constraints <- readIORefFromLens constraintsL
  sat constraints

sat :: (HasLogFunc env, HasConstraints env) => [Constraint] -> RIO env Bool
sat constraints = do
  clauses <- generateClauses constraints
  goals  <- generateGoals
  results <-withRunInIO $ \run -> do
    resolve clauses goals
  return (not $ null results)

generateClauses :: (HasLogFunc env, HasConstraints env) => [Constraint] -> RIO env [Clause]
generateClauses constraints = do
  allCons <- readIORefFromLens constraintsL
  let heads = L.nub (map cstHead allCons)
  let mkClause head =
         let matchHead = filter ((== head) . cstHead) constraints
             bodies = map cstBody matchHead
         in  Clause (Struct head [varHead]) bodies
  let clauses = map mkClause heads
  return clauses

generateGoals :: (HasLogFunc env, HasConstraints env) => RIO env [Term]
generateGoals  = do
  allCons <- readIORefFromLens constraintsL
  let heads = L.nub $ map cstHead allCons
  let goals  = map (\h -> Struct h [Wildcard]) heads
  return goals

simplifyShow :: Clause -> Text
simplifyShow clause =
  let text = T.pack (show clause)
      no_type_of = TP.replace "typeof_" T.empty text
      no_haskell = TP.replace "haskell_" T.empty no_type_of
  in no_haskell
