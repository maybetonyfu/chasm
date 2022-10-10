{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Typing where

import Data.List (intersperse)
import Data.String.QQ
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import RIO
import Range

data ConstrAssemble = ConstrAssemble
  { logFun :: LogFunc,
    getConstr :: IORef [Constr],
    counter :: IORef Int
  }

data Constr = Constr
  { reason :: Text,
    code :: Text,
    range :: Range
  }

instance HasLogFunc ConstrAssemble where
  logFuncL = lens logFun (\x y -> x {logFun = y})

class HasTyping f where
  matchTerm :: Text -> f SrcSpanInfo -> RIO ConstrAssemble ()


fresh :: RIO ConstrAssemble Text
fresh = do
  s <- ask
  number <- readIORef (counter s)
  modifyIORef (counter s) (+ 1)
  return (textDisplay number)


eq :: Text -> Text -> Text
eq a b =
  mconcat
    [ "eq(",
      textDisplay a,
      ", ",
      textDisplay b,
      ")"
    ]

app :: Text -> [Text] -> Text
app f args =
  mconcat
    [ textDisplay f,
      "(",
      mconcat (intersperse ", " args),
      ")"
    ]

instance HasTyping Module where
  matchTerm t (Module _ _ _ _ decls) = mapM_ (matchTerm t) decls
  matchTerm _ _ = undefined

instance HasTyping Decl where
  matchTerm t (PatBind srcspan pat rhs maybeWheres) = do
    vl <- fresh
    vr <- fresh
    matchTerm vl pat
    matchTerm vr rhs
    logInfo . display $ eq vl vr
  -- matchTerm t (FunBind srcspan matches) = do
  --   mapM_ (makeSlices p) matches
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
  matchTerm t (UnGuardedRhs srcspan exp) = return ()
  matchTerm t node = error ("Node type not support: " ++ show node)

instance HasTyping Exp where
  matchTerm term (InfixApp srcspan e1 op e2) = do
    return ()
  matchTerm term (App srcspan e1 e2) = do
    var1 <- fresh
    var2 <- fresh
    matchTerm var1 e1
    matchTerm var2 e2
    let code = eq term (app var1 [var2])
    logInfo (display code)
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
  matchTerm term (Lit _ _) = do
    return ()
  matchTerm term (Var _ qname) = do
    return ()
  matchTerm term node = do
    return ()

instance HasTyping Literal where
  matchTerm term (Char srcspan _ _) = do
    logInfo (displayShow term <> " = " <> "char")
  matchTerm term (String srcspan _ _) = do
    logInfo (displayShow term <> " = " <> "string")
  matchTerm term (Int srcspan _ _) = do
    logInfo (displayShow term <> " = " <> "int")
  matchTerm term (Frac srcspan _ _) = do
    logInfo (displayShow term <> " = " <> "float")
  matchTerm _ node = error ("Node type not support: " ++ show node)

main :: IO ()
main = runSimpleApp $ do
  logFunc <- view logFuncL
  let contents = testSample
  let pResult = parseModuleWithMode parseMode contents
      parseMode = defaultParseMode {parseFilename = "MyFile"}
  case pResult of
    ParseOk hModule -> do
      logInfo "OK"
      ioList <- newIORef []
      ioInteger <- newIORef 0
      constrObj <- runRIO (ConstrAssemble logFunc ioList ioInteger) (matchTerm "" hModule >> ask)
      return ()
    ParseFailed srcLoc message ->
      logInfo "Parsing Failed"

testSample :: String
testSample =
  [s|
x = 3
y = M.x
|]
