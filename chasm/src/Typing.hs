{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Typing where

import Data.String.QQ
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import RIO
import RIO.List
import Range
import Types

class HasTyping f where
  matchTerm :: (HasLogFunc env) => Text -> f SrcSpanInfo -> RIO env ()

instance HasTyping Module where
  matchTerm t (Module _ _ _ _ decls) = mapM_ (matchTerm t) decls
  matchTerm _ _ = undefined

instance HasTyping Decl where
  matchTerm t (PatBind srcspan pat rhs maybeWheres) = do
    return ()
  matchTerm t (FunBind srcspan matches) = do
    mapM_ (matchTerm t) matches
  matchTerm t node = error ("Node type not support: " ++ show node)

instance HasTyping Pat where
  matchTerm t (PVar _ name) = do
    logInfo (displayShow name)
  matchTerm t (PInfixApp _ p1 op p2) = return ()
  matchTerm t (PApp _ _ ps) = return ()
  matchTerm t (PTuple _ _ ps) = return ()
  matchTerm t (PList _ ps) = return ()
  matchTerm t (PParen _ p) = return ()
  matchTerm t (PAsPat _ name p) = return ()
  matchTerm t PLit {} = return ()
  matchTerm t PWildCard {} = return ()
  matchTerm t node = error $ "Node type not supported: " ++ show node

instance HasTyping Rhs where
  matchTerm t (UnGuardedRhs srcspan exp) = matchTerm t exp
  matchTerm t node = error ("Node type not support: " ++ show node)

instance HasTyping Exp where
  matchTerm term (InfixApp srcspan e1 op e2) = do
    return ()
  matchTerm term (App srcspan e1 e2) = do
    return ()
  matchTerm term (NegApp srcspan e) = do
    return ()
  matchTerm term (Lambda srcspan pats e) = do
    return ()
  matchTerm term (Let srcspan bind e) = do
    return ()
  matchTerm term (If srcspan e1 e2 e3) = do
    return ()
  matchTerm term (Case srcspan exp alts) = do
    return ()
  matchTerm term (Tuple srcspan _ exps) = do
    return ()
  matchTerm term (List srcspan exps) = do
    return ()
  matchTerm term (Paren srcspan e) = do
    return ()
  matchTerm term (LeftSection srcspan e _) = do
    return ()
  matchTerm term (RightSection srcspan _ e) = do
    return ()
  matchTerm term (Lit srcspan l) = do
    return ()
  matchTerm term (Var _ qname) = do
    return ()
  matchTerm term node = do
    return ()

instance HasTyping Match where
  matchTerm t (Match srcspan name pats rhs maybeWheres) = do
    return ()
  matchTerm t (InfixMatch srcInfo pat name pats rhs maybeWheres) =
    matchTerm t (Match srcInfo name (pat : pats) rhs maybeWheres)

instance HasTyping Literal where
  matchTerm term (Char srcspan _ _) = do
    logInfo . display $  term `eq` "char"

  matchTerm term (String srcspan _ _) = do
    logInfo . display $ term `eq` "string"

  matchTerm term (Int srcspan _ _) = do
    logInfo . display $ term `eq` "int"

  matchTerm term (Frac srcspan _ _) = do
    logInfo . display $ term `eq` "float"

  matchTerm _ node = error ("Node type not support: " ++ show node)



eq :: Text -> Text -> Text
eq a b =
  mconcat
    [
      textDisplay a,
      " = ",
      textDisplay b
    ]

app :: Text -> [Text] -> Text
app f args =
  mconcat
    [ textDisplay f,
      "(",
      mconcat (intersperse ", " args),
      ")"
    ]
