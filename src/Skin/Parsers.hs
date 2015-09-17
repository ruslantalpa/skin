--{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, OverloadedStrings, FlexibleContexts #-}
module Skin.Parsers
-- ( parseGetRequest
-- , pSelect
-- , pField
-- , pRequestInclude
-- )
where
import           Text.ParserCombinators.Parsec hiding (many, (<|>))
--import Text.Parsec.Text
--import Text.Parsec hiding (many, (<|>))
--import Text.Parsec.Prim hiding (many, (<|>))
import           Control.Applicative
--import Control.Monad
--import qualified Data.Text                     as T
import           Data.Tree
import           Network.Wai                   (Request, pathInfo, queryString)
import           Skin.Types
--import qualified Data.ByteString.Char8 as C
--import           Control.Monad
--import           Data.Foldable                 (foldrM)
import           Data.List                     (delete, find)
import           Data.Maybe
import           Data.String.Conversions       (cs)
--import qualified Data.ByteString.Char8 as C

--buildRequest :: String -> String -> [(String, String)] -> Either P.ParseError Request
parseGetRequest :: Request -> Either ParseError ApiRequest
parseGetRequest httpRequest =
    foldr addFilter <$> apiRequest <*> flts
    where
        apiRequest = parse (pRequestInclude rootTableName) "failed to parse include" $ cs includeStr
        flts = mapM pRequestFilter whereFilters
        rootTableName = cs $ head $ pathInfo httpRequest -- TODO unsafe head
        qString = [(cs k, cs <$> v)|(k,v) <- queryString httpRequest]
        includeStr = fromMaybe "*" $ fromMaybe (Just "*") $ lookup "include" qString --in case the parametre is missing or empty we default to *
        whereFilters = [ (k, fromJust v) | (k,v) <- qString, k `notElem` ["include"], isJust v ]

pRequestInclude :: String -> Parser ApiRequest
pRequestInclude rootNodeName = do
    fieldTree <- pFieldForest
    return $ foldr treeEntry (Node (RequestNode rootNodeName [] []) []) fieldTree
    where
        treeEntry :: Tree SelectItem -> Tree RequestNode -> Tree RequestNode
        treeEntry (Node fld@(p,_) fldForest) (Node rNode rForest) =
            case fldForest of
                [] -> Node (rNode {fields=fld:fields rNode}) rForest
                _  -> Node rNode (foldr treeEntry (Node (RequestNode (head p) [] []) []) fldForest:rForest)

--TODO handle json columns
pRequestFilter :: (String, String) -> Either ParseError (Path, Filter)
pRequestFilter (k, v) = (,) <$> path <*> (Filter <$> fld <*> op <*> val)
    where
        treePath = parse pTreePath "failed to parser tree path" k
        opVal = parse pOpValueExp "failed to parse filter" v
        path = fst <$> treePath
        fld = snd <$> treePath
        op = fst <$> opVal
        val = snd <$> opVal

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

ws :: Parser String
ws = many (oneOf " \t")

--lexeme :: Parser String -> Parser String
--lexeme :: Text.Parsec.Prim.ParsecT String () Data.Functor.Identity.Identity a -> Text.Parsec.Prim.ParsecT String () Data.Functor.Identity.Identity a
--lexeme :: Text.Parsec.Prim.ParsecT String () Data.Functor.Identity.Identity Char -> Text.Parsec.Prim.ParsecT String () Data.Functor.Identity.Identity Char
lexeme p = ws *> p <* ws

pTreePath :: Parser (Path,Field)
pTreePath = do
    p <- pFieldName `sepBy` pDelimiter
    f <- pField
    return (p, f)


pFieldForest :: Parser [Tree SelectItem]
pFieldForest = pFieldTree `sepBy` lexeme (char ',')

pFieldTree :: Parser (Tree SelectItem)
pFieldTree =
    try (
        do
            fld <- pSelect
            _ <- lexeme $ char '('
            subforest <- pFieldForest
            _ <- lexeme $ char ')'
            return (Node fld subforest))
    <|> (
        do
            fld <- pSelect
            return (Node fld [])
    )

pFieldName :: Parser String
pFieldName =  string "*" *> pure "*"
          <|> many (letter <|> digit <|> oneOf "_")
          <?> "field name (* or [a..z0..9_])"
pField :: Parser Field
pField = pFieldName `sepBy` (try (string "->>") <|> string "->")

pSelect :: Parser SelectItem
pSelect = lexeme $ do
    n <- pField
    v <- optionMaybe (string "::" >> many letter)
    return (n, v)

pOperator :: Parser Operator
pOperator =  try (string "eq" *> pure OpEQ)
         <|> try (string "gt" *> pure OpGT)
         <|> try (string "lt" *> pure OpLT)
         <?> "operator (eq, gt, ...)"

-- pInt :: Parser Int
-- pInt = try (liftA read (many (char ' ') *> many1 digit <* many (char ' '))) <?> "integer"

--pValue :: Parser Value
--pValue = (VInt <$> try (pInt <* eof))
--      <|>(VString <$> many anyChar)
pValue :: Parser Value
pValue = VString <$> many anyChar

pDelimiter :: Parser Char
pDelimiter = char '.' <?> "delimiter (.)"

pOpValueExp :: Parser (Operator, Value)
pOpValueExp = liftA2 (,) pOperator (pDelimiter *> pValue)
