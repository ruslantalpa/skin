module Skin.Parsers
where

import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Control.Applicative
import Control.Monad
import Skin.Types
import Data.Tree

-- id,name
-- id,name,client(id,name)
-- tasks.id=eq.1
ws :: Parser String
ws = many (oneOf " \t")

lexeme p = ws *> p <* ws
--data RequestNode = RequestNode {nodeName::String, fields::[Field], filters::[Filter]} deriving (Show, Eq)
pFieldForest :: Parser [Tree String]
pFieldForest = pFieldTree `sepBy` (lexeme $ char ',')

pTreePath :: Parser ([String],String)
pTreePath = do
    fullPath <- pFieldName `sepBy` char '.'
    return (init fullPath, last fullPath)

pRequestInclude :: String -> Parser Request
pRequestInclude rootNodeName = do
    fieldTree <- pFieldForest
    return $ foldr treeEntry (Node (RequestNode rootNodeName [] []) []) fieldTree
    where
        treeEntry (Node fldName fldForest) (Node rNode rForest) =
            case fldForest of
                [] -> Node (rNode {fields=fldName:fields rNode}) rForest
                _  -> Node rNode (foldr treeEntry (Node (RequestNode fldName [] []) []) fldForest:rForest)


pFieldTree :: Parser (Tree Field)
pFieldTree =
    try (
        do
            field <- pFieldName
            lexeme $ char '('
            subforest <- pFieldForest
            lexeme $ char ')'
            return (Node field subforest))
    <|> (
        do
            field <- pFieldName
            return (Node field [])
    )

pFieldName :: Parser String
pFieldName =  lexeme $ many $ letter <|> digit <|> oneOf "-_"

pOp :: Parser Operator
pOp =  (string "eq" *> pure OpEQ)
   <|> (string "gt" *> pure OpGT)
   <|> (string "lt" *> pure OpLT)

pInt :: Parser Int
pInt = liftA read (many (char ' ') *> many1 digit <* many (char ' '))

pValue :: Parser Value
pValue = (VInt <$> try (pInt <* eof))
      <|>(VString <$> many anyChar)

pOpValueExp :: Parser (Operator, Value)
pOpValueExp = do
    op <- pOp
    char '.'
    val <- pValue
    return (op, val)
