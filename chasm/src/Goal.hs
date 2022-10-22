{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Goal  where
import RIO
import Language.Prolog
import qualified Text.Parsec as P

unit = Struct "unit" []

atomFrom :: String -> Term
atomFrom x = Struct x []

varFrom = var

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

main :: IO ()
main =  runSimpleApp $ do
  -- let t = P.parse term "" "X = Y"
  -- case t of
  --   Left _ -> logInfo "Error"
  --   Right z -> do
  --     logInfo (displayShow z)
  --     logInfo (displayShow (isStruct z))
  --     logInfo (displayShow (structFunctor z))
  --     logInfo (displayShow (structArgs z))
  -- let myt = Wildcard
  -- logInfo (displayShow myt)
  p <- withRunInIO $ \run -> do
    let clauseId = Clause (funFrom (atomFrom "id") [varFrom "X", varFrom "Y"])  [eq (varFrom "X") (varFrom "Y")]
    run . logInfo . displayShow $ clauseId
    resolve [clauseId] [atomFrom "true"]
  logInfo (displayShow p)


