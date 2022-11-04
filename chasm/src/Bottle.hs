{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Bottle where

import Language.Haskell.Exts
import Data.Aeson
import qualified Data.Aeson.Types as Text
import Data.String.QQ
import RIO hiding ((<|>))
import RIO.Process
import RIO.FilePath ((</>), normalise)
import qualified RIO.List as L
import qualified RIO.Text as T
import qualified RIO.ByteString as B
import Text.Parsec as P
import qualified RIO.Process as PR
import Types
import Lenses

moduleName :: Module a -> Text
moduleName (Module _ (Just (ModuleHead _ (ModuleName _ name) _ _)) _ _ _) = T.pack name
moduleName (Module _ Nothing _ _ _) = "Main"
moduleName _ = error "Not a module"

optimizeBottleDrops :: [Load] -> Bottle -> Bottle
optimizeBottleDrops loads b@(Bottle name path drops) =
  let matchingLoad = L.find ((== name) . loadName) loads
   in case matchingLoad of
        Nothing -> b
        Just l ->
          case loadVars l of
            Everything -> b
            Inc vs -> Bottle name path (filter ((`elem` vs) . fst) drops)
            Exc vs -> Bottle name path (filter ((`notElem` vs) . fst) drops)

loadBottles :: (HasProcessContext env, HasLogFunc env, HasBottle env, HasBasicInfo env, HasLoad env, HasTargetName env) => RIO env ()
loadBottles = do
  (root, path) <- view basicInfoL
  let root' = normalise root
  let path' = normalise path
  let runCommand command = do
        logDebug (display command)
        (_, commandOut, _) <- readProcess command
        logDebug "-----------------------"
        logDebug (displayShow commandOut)
        logDebug "-----------------------"
        let outText = decodeUtf8' (toStrictBytes commandOut)
        case outText of
          Left e -> error "GHC output is not valid utf8"
          Right t -> return (parseGhcTypeCheck t)
  let args = ["-fno-code", "-fforce-recomp", "-ddump-types", "-ddump-json", "-i=" ++ root', root' </> path']
  bottles <- PR.proc "ghc" args runCommand

  targetName <- readIORefFromLens targetNameL
  loads <- readIORefFromLens loadL
  let externalBottles = filter (\b -> bottleName b /= targetName) bottles
  let optimalBottles = map (optimizeBottleDrops loads) externalBottles

  bottleHandle <- view bottleL
  writeIORef bottleHandle optimalBottles


-- main :: IO ()
-- main = runSimpleApp $ do
--   -- let result1 = P.parse parseCompilerMessageHead "" "[1 of 2] Compiling Test2            ( Test2.hs, nothing )"
--   -- let result2 = P.parse parseCompilerMessageHead "" "[2 of 2] Compiling Data.Test        ( Data/Test.hs, nothing )"
--   -- logInfo (displayShow result1)
--   -- logInfo (displayShow result2)
--   -- let sigLine1 = P.parse bodyTypeSig "" "  (=.=) :: forall {a}. Num a => a -> a -> a\n"
--   -- logInfo (displayShow sigLine1)
--   -- let bodyText = P.parse parseCompilerMessageBody "" "TYPE SIGNATURES\n  fa :: forall a. A a => a -> a\n  x :: Integer\n  y :: Integer\nTYPE CONSTRUCTORS\n  class A{1} :: * -> Constraint\nCOERCION AXIOMS\n  axiom Test2.N:A :: A a = a -> a\nCLASS INSTANCES\n  instance A Int -- Defined at Test2.hs:7:10\nDependent modules: []\nDependent packages: [base-4.16.3.0, ghc-bignum-1.2, ghc-prim-0.8.0]"
--   -- logInfo (displayShow bodyText)
--   bottles <- readDependency "c:/Users/sfuu0016/Projects/chasm" "Test.hs"
--   logInfo (displayShow bottles)

newtype CompilerMessage = CompilerMessage (Maybe Text) deriving (Show)

getCompilerMessage (CompilerMessage maybetext) = maybetext

instance FromJSON CompilerMessage where
  parseJSON = withObject "CompilerMessage" go
    where
      go v = do
        severity :: String <- v .: "severity"
        doc' <- v .: "doc"
        if severity == "SevOutput"
          then return (CompilerMessage (Just doc'))
          else return (CompilerMessage Nothing)

seperateHeader :: [CompilerMessage] -> [(Text, Text)]
seperateHeader [] = []
seperateHeader (CompilerMessage (Just e):xs) =
  let isHeader = T.isPrefixOf "[" e
  in if isHeader
        then
          let (body, rest) = seperateBody xs
              safeBody = if T.null body then "TYPE SIGNATURES\n" else body
          in (e, safeBody):seperateHeader rest
        else error "Should always start with header"

seperateBody :: [CompilerMessage] -> (Text, [CompilerMessage])
seperateBody [] = ("", [])
seperateBody (CompilerMessage Nothing: xs) = seperateBody xs
seperateBody ts@(CompilerMessage (Just e): xs) =
  let isHeader = T.isPrefixOf "[" e
  in if isHeader
        then ("", ts)
        else let continued = seperateBody xs
             in (e <> fst continued, snd continued)

parseGhcTypeCheck :: Text -> [Bottle]
parseGhcTypeCheck input =
  let inputLines = T.lines input
      utf8Lines = fmap (fromStrictBytes . encodeUtf8) inputLines
      messages :: [CompilerMessage]
      messages = mapMaybe decode utf8Lines
      pickText (CompilerMessage e) [] = [[e]]
      pickText (CompilerMessage e) ([x] : xs) = ([e, x] : xs)
      pickText (CompilerMessage e) xs = ([e]:xs)
      sepd ::[(Text, Text)]
      sepd = seperateHeader messages
   in fmap parseHeadAndBody sepd

parseHeadAndBody :: (Text, Text) -> Bottle
parseHeadAndBody (head, body) =
  let headR = P.parse parseCompilerMessageHead "" head
      bodyR = P.parse parseCompilerMessageBody "" body
   in case (headR, bodyR) of
        (Left _, _) -> error "Cannot parse head"
        (Right (head1, head2), Left _) -> Bottle head1 (T.unpack head2) []
        (Right (head1, head2), Right bs) ->
          Bottle
            { bottleName = head1,
              bottlePath = T.unpack head2,
              bottleDrops = bs
            }

parseCompilerMessageHead :: Parsec Text () (Text, Text)
parseCompilerMessageHead = do
  -- [1 of 2] Compiling Test2            ( Test2.hs, nothing )
  manyTill anyChar (char ']')
  skipMany space
  string "Compiling"
  skipMany space
  moduleName <- many1 (alphaNum <|> char '.')
  skipMany space
  char '('
  skipMany space
  modulePath <- many1 (alphaNum <|> oneOf ".:-\\/")
  char ','
  return (T.pack moduleName, T.pack modulePath)

parseCompilerMessageBody :: Parsec Text () [(Text, Type SrcSpanInfo)]
parseCompilerMessageBody = do
  -- TYPE SIGNATURES
  --   fa :: forall a. A a => a -> a
  --   x :: Integer
  --   y :: Integer
  -- manyTill anyChar (P.try $ string "TYPE SIGNATURES")
  string "TYPE SIGNATURES"
  endOfLine
  typeSigs <- P.many bodyTypeSig
  return typeSigs

haskellIdentifier :: Parsec Text () Text
haskellIdentifier = do
  firstLetter <- lower
  restLetters <- P.many (alphaNum <|> char '\'')
  return (T.pack $ firstLetter : restLetters)

haskellOperator :: Parsec Text () Text
haskellOperator = do
  char '('
  name <- many1 (oneOf "=./\\!*&$#%<>|+-%")
  char ')'
  return (T.pack name)

haskellName = haskellIdentifier <|> haskellOperator

bodyTypeSig :: Parsec Text () (Text, Type SrcSpanInfo)
bodyTypeSig = do
  many1 space
  name <- haskellName
  space
  string "::"
  space
  sig <- manyTill anyChar endOfLine
  let hType = fromParseResult (parseType sig)
  return (name, hType)

sampleOutput :: Text
sampleOutput =
  [s|
{"span": null,"doc": "[1 of 2] Compiling Test2            ( Test2.hs, nothing )","severity": "SevOutput","reason": null}
{"span": null,"doc": "TYPE SIGNATURES\n  fa :: forall a. A a => a -> a\n  x :: Integer\n  y :: Integer\nTYPE CONSTRUCTORS\n  class A{1} :: * -> Constraint\nCOERCION AXIOMS\n  axiom Test2.N:A :: A a = a -> a\nCLASS INSTANCES\n  instance A Int -- Defined at Test2.hs:7:10\nDependent modules: []\nDependent packages: [base-4.16.3.0, ghc-bignum-1.2, ghc-prim-0.8.0]","severity": "SevOutput","reason": null}
{"span": null,"doc": "[2 of 2] Compiling Test             ( Test.hs, nothing )","severity": "SevOutput","reason": null}
{"span": {"file": "Test.hs","startLine": 7,"startCol": 10,"endLine": 7,"endCol": 14},"doc": "* Couldn't match expected type `Integer' with actual type `Bool'\n* In the second argument of `(==)', namely `True'\n  In the expression: y == True\n  In an equation for `b': b = y == True","severity": "SevError","reason": null}
|]
