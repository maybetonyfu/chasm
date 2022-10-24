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

unit = Struct "unit" []
varHead = varFrom "T"

atomFrom :: String -> Term
atomFrom x = Struct x []

varFrom = var

typeOf :: String -> Term -> Term
typeOf x t = Struct ("typeof_" ++ x) [t]

funFrom :: Term -> [Term] -> Term
funFrom  x [] = unit
funFrom fname (x:xs) = Struct "function" [fname, x, funFrom Wildcard xs]

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

solve :: (HasConstraints env, HasLogFunc env) => RIO env Bool
solve = do
  constraints <- readIORefFromLens constraintsL
  let heads = L.nub $ map cstHead constraints
  let mkClause head =
         let matchHead = filter ((== head) . cstHead) constraints
             bodies = map cstBody matchHead
         in  Clause (Struct head [varHead]) bodies
  let clauses = map mkClause heads
  mapM_ (logInfo . displayShow) clauses
  let goals  = map (\h -> Struct h [Wildcard]) heads
  results <-withRunInIO $ \run -> do
    resolve clauses goals
  return (not $ null results)

-- main :: IO ()
-- main =  runSimpleApp $ do
--   p <- withRunInIO $ \run -> do
--     let clauseId = Clause (funFrom (atomFrom "id") [varFrom "X", varFrom "Y"])  [eq (varFrom "X") (varFrom "Y")]
--     run . logInfo . displayShow $ clauseId
--     resolve [clauseId] [atomFrom "true"]
--   logInfo (displayShow p)


