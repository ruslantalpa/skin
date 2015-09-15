--{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, OverloadedStrings, FlexibleContexts #-}
module Skin.Parsers
( parseGetRequest
)
where
import Text.ParserCombinators.Parsec hiding ((<|>), many)
--import Text.Parsec.Text
--import Text.Parsec hiding (many, (<|>))
--import Text.Parsec.Prim hiding (many, (<|>))
import Control.Applicative
--import Control.Monad
import qualified Data.Text as T
import Skin.Types
import Data.Tree
import qualified Network.Wai as Wai
--import qualified Data.ByteString.Char8 as C
import Data.Maybe
import Data.Foldable (foldrM)
import Control.Monad
import Data.List ( delete, find)
import Data.String.Conversions (cs)
--import qualified Data.ByteString.Char8 as C

--buildRequest :: String -> String -> [(String, String)] -> Either P.ParseError Request
parseGetRequest :: Wai.Request -> Either ParseError ApiRequest
parseGetRequest httpRequest =
    case request of
        Right r -> foldrM insertFilter r flts
        Left e -> Left e
    where
        request = parse (pRequestInclude rootTableName) "failed to parse include" $ cs includeStr
        insertFilter :: Either ParseError (Path, Filter) -> ApiRequest -> Either ParseError ApiRequest
        insertFilter (Right (path, flt)) node = Right $ addFilter (path, flt) node
        insertFilter (Left e) _ = Left e
        parseFilter :: (String, String) -> Either ParseError (Path, Filter)
        parseFilter (k, v) = (,) <$> path <*> (Filter <$> fld <*> op <*> val)
            where
                treePath = parse pTreePath "failed to parser tree path" k
                opVal = parse pOpValueExp "failed to parse filter" v
                path = fst <$> treePath
                fld = snd <$> treePath
                op = fst <$> opVal
                val = snd <$> opVal
        flts = map parseFilter whereFilters

        rootTableName = T.unpack $ head $ Wai.pathInfo httpRequest
        qString = [(cs k, cs <$> v)|(k,v) <- (Wai.queryString httpRequest)]
        includeStr = fromJust $ join $ lookup "include" qString
        whereFilters = [ (k, fromMaybe "" v) | (k,v) <- qString, k `notElem` ["include"] ]

addFilter :: (Path, Filter) -> ApiRequest -> ApiRequest
addFilter ([], flt) (Node rn@(RequestNode {filters=flts}) forest) = Node (rn {filters=flt:flts}) forest
addFilter (path, flt) (Node rn forest) =
    case targetNode of
        Nothing -> Node rn forest -- the filter is silenty dropped in the Request does not contain the required path
        Just tn -> Node rn (addFilter (remainingPath, flt) tn:restForest)
    where
        targetNodeName:remainingPath = path
        (targetNode,restForest) = splitForest targetNodeName forest
        splitForest name forst =
            case maybeNode of
                Nothing -> (Nothing,forest)
                Just node -> (Just node, delete node forest)
            where maybeNode = find ((name==).nodeName.rootLabel) forst
-- id,name
-- id,name,client(id,name)
-- tasks.id=eq.1
ws :: Parser String
ws = many (oneOf " \t")

--lexeme :: Parser String -> Parser String
lexeme p = ws *> p <* ws

pTreePath :: Parser ([String],String)
pTreePath = do
    fullPath <- pFieldName `sepBy` char '.'
    return (init fullPath, last fullPath)

pRequestInclude :: String -> Parser ApiRequest
pRequestInclude rootNodeName = do
    fieldTree <- pFieldForest
    return $ foldr treeEntry (Node (RequestNode rootNodeName [] []) []) fieldTree
    where
        treeEntry (Node fldName fldForest) (Node rNode rForest) =
            case fldForest of
                [] -> Node (rNode {fields=fldName:fields rNode}) rForest
                _  -> Node rNode (foldr treeEntry (Node (RequestNode fldName [] []) []) fldForest:rForest)

pFieldForest :: Parser [Tree String]
pFieldForest = pFieldTree `sepBy` lexeme (char ',')

pFieldTree :: Parser (Tree Field)
pFieldTree =
    try (
        do
            fld <- pFieldName
            _ <- lexeme $ char '('
            subforest <- pFieldForest
            _ <- lexeme $ char ')'
            return (Node fld subforest))
    <|> (
        do
            fld <- pFieldName
            return (Node fld [])
    )

pFieldName :: Parser String
pFieldName =  try (lexeme $ many $ letter <|> digit <|> oneOf "-_") <?> "field name (a..z, 0..9, -_)"

pOp :: Parser Operator
pOp =  try (
       try (string "eq" *> pure OpEQ)
   <|> try (string "gt" *> pure OpGT)
   <|> try (string "lt" *> pure OpLT)
   ) <?> fail "operator (eq, gt, ...)"

pInt :: Parser Int
pInt = try (liftA read (many (char ' ') *> many1 digit <* many (char ' '))) <?> "integer"

--pValue :: Parser Value
--pValue = (VInt <$> try (pInt <* eof))
--      <|>(VString <$> many anyChar)
pValue :: Parser Value
pValue = VString <$> many anyChar

pDelimiter :: Parser Char
pDelimiter = char '.' <?> "delimiter (.)"

pOpValueExp :: Parser (Operator, Value)
pOpValueExp = liftA2 (,) pOp (pDelimiter *> pValue)
