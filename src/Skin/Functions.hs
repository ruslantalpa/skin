{-# LANGUAGE OverloadedStrings #-}
module Skin.Functions
( dbRequestToQuery
, requestNodeToQuery
, addJoinConditions
--, buildRelations
, addRelations
--, addField
, buildRequest
) where

import qualified Text.ParserCombinators.Parsec as P hiding ((<|>), many)
import Skin.Types
import Skin.Parsers
import Data.Tree
import Data.Foldable

import Control.Applicative
import Data.List (intercalate, intersperse, nub, find, delete)
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import qualified Data.Text as T


buildRequest :: String -> String -> [(String, String)] -> Either P.ParseError Request
buildRequest rootTableName includeStr whereFilters =
    case request of
        Right r -> foldrM insertFilter r filters
        Left e -> Left e
    where
        request = P.parse (pRequestInclude rootTableName) "failed to parse include" includeStr
        insertFilter :: Either P.ParseError (Path, Filter) -> Request -> Either P.ParseError Request
        insertFilter (Right (path, flt)) node = Right $ addFilter node path flt
        insertFilter (Left e) _ = Left e
        parseFilter :: (String, String) -> Either P.ParseError (Path, Filter)
        parseFilter (k, v) = (,) <$> path <*> (Filter <$> field <*> op <*> val)
            where
                treePath = P.parse pTreePath "failed to parser tree path" k
                opVal = P.parse pOpValueExp "failed to parse filter" v
                path = fst <$> treePath
                field = snd <$> treePath
                op = fst <$> opVal
                val = snd <$> opVal
        filters = map parseFilter whereFilters

addFilter :: Request -> Path -> Filter -> Request
addFilter (Node rn@(RequestNode {filters=flts}) forest) [] flt = Node (rn {filters=flt:flts}) forest
addFilter (Node rn forest) path flt =
    case targetNode of
        Nothing -> Node rn forest
        Just tn -> Node rn (addFilter tn remainingPath flt:restForest)
    where
        targetNodeName:remainingPath = path
        (targetNode,restForest) = splitForest targetNodeName forest
        splitForest name forest =
            case maybeNode of
                Nothing -> (Nothing,forest)
                Just node -> (Just node, delete node forest)
            where maybeNode = find ((name==).nodeName.rootLabel) forest

requestNodeToQuery ::[Table] -> [Column] -> RequestNode -> Maybe Query
requestNodeToQuery tables columns (RequestNode name fields filters) =
    Select <$> mainTable <*> select <*> from <*> qwhere <*> rel
    where mainTable = find ((name==).tblName) tables
          maybeColumns = map (\f->find (\c->colTable c == name && colName c == f) columns) fields
          select = if all isJust maybeColumns then pure (catMaybes maybeColumns) else Nothing
          from = pure []
          maybeConditions = map (filterToCondition tables columns name) filters
          qwhere = if all isJust maybeConditions then pure (catMaybes maybeConditions) else Nothing
          rel = pure Nothing
          filterToCondition :: [Table] -> [Column] -> String -> Filter -> Maybe Condition
          filterToCondition tables columns table (Filter fld op val) =
              Condition <$> column <*> pure op <*> pure val
              where column = find (\c->colTable c == table && colName c == fld) columns

addRelations :: [RelationEntry] -> Maybe (Tree Query) -> Tree Query -> Tree Query
addRelations allRelations parentNode node@(Node query@(Select {qMainTable=table}) forest) =
    case parentNode of
        Nothing -> Node query {qRelation=Just Root} updatedForest
        (Just Node{rootLabel=Select{qMainTable=parentTable}}) -> Node query {qRelation=rel} updatedForest
            where
                rel = findRelation allRelations (tblName table) (tblName parentTable)
    where
        updatedForest = map (addRelations allRelations (Just node)) forest
        findRelation :: [RelationEntry] -> String -> String -> Maybe Relation
        findRelation relations t1 t2 = getRelation <$> find (\(f,s,rs,r)->t1==f&&t2==s) relations
            where getRelation (_,_,_,r) = r

addJoinConditions :: Tree Query -> Maybe (Tree Query)
addJoinConditions (Node query@(Select{qMainTable=table, qFrom=from, qWhere=conditions, qRelation=relation}) forest) =
    case relation of
        Nothing -> empty -- blow up
        Just Root -> Node <$> pure updatedQuery <*> updatedForest
        Just (Child relationColumn) -> Node <$> pure updatedQuery{qWhere=getJoinCondition relationColumn:qWhere updatedQuery} <*> updatedForest
        Just (Parent relationColumn) -> Node <$> pure updatedQuery <*> updatedForest
        Just (Many relationColumn1 relationColumn2) -> Node <$> pure updatedQuery{qFrom=linkTable:qFrom updatedQuery, qWhere=cond1:cond2:qWhere updatedQuery} <*> updatedForest
            where
                cond1 = getJoinCondition relationColumn1
                cond2 = getJoinCondition relationColumn2
                linkTable = Table (colTable relationColumn1)
    where
        updatedQuery = query {qWhere=parentJoinConditions++conditions, qFrom=from++parentTables}
        maybeUpdatedForest = map addJoinConditions forest
        updatedForest = if all isJust maybeUpdatedForest then pure (catMaybes maybeUpdatedForest) else Nothing
        parentJoinConditions = map (getJoinCondition.snd) parents
        parentTables = map fst parents
        parents = mapMaybe (getParents.rootLabel) forest
        getParents q@(Select{qRelation=rel@(Just (Parent relationColumn))}) = Just (qMainTable q, relationColumn)
        getParents _ = Nothing
        getJoinCondition relationColumn = Condition relationColumn OpEQ (VForeignKey ((fromJust.colFk) relationColumn))

dbRequestToQuery :: DbRequest -> String
dbRequestToQuery (Node (Select mainTable columns tables conditions relation) forest) =
    unwords [
        if relation == Just Root
        then "SELECT pg_catalog.count(t),array_to_json(array_agg(row_to_json(t)))::CHARACTER VARYING AS json FROM ("
        else "",
        withsStr,
        "\nSELECT", intercalate ", " (map colToStr columns ++ selects),
        "\nFROM", intercalate ", " (tblName mainTable:map tblName tables),
        whereStr,
        if relation == Just Root
        then ") t;"
        else ""

    ]
    where withsStr = if null withs then "" else "WITH " ++ intercalate ", " withs
          whereStr = if null conditions then "" else "WHERE " ++ intercalate " AND " ( map conditionToStr conditions )
          (withs, selects) = foldr getQueryParts ([],[]) forest
          getQueryParts (Node query@(Select{qMainTable=table, qRelation=(Just (Child _))}) forest) (w,s) = (w,sel:s)
            where name = tblName table
                  sel = "(SELECT array_to_json(array_agg(row_to_json("++name++"))) FROM (" ++ dbRequestToQuery (Node query forest) ++ ") "++name++" ) AS "++name
          getQueryParts (Node query@(Select{qMainTable=table, qRelation=(Just (Parent _))}) forest) (w,s) = (wit:w,sel:s)
            where name = tblName table
                  sel = "row_to_json("++name++".*) AS "++name --TODO must be singular
                  wit = name ++ " AS ( " ++ dbRequestToQuery (Node query forest) ++ " )"
          getQueryParts (Node query@(Select{qMainTable=table, qRelation=(Just (Many _ _))}) forest) (w,s) = (w,sel:s)
            where name = tblName table
                  sel = "(SELECT array_to_json(array_agg(row_to_json("++name++"))) FROM (" ++ dbRequestToQuery (Node query forest) ++ ") "++name++" ) AS "++name

colToStr :: Column -> String
colToStr col = colTable col ++ "." ++ colName col

conditionToStr :: Condition -> String
conditionToStr (Condition col op val) = colToStr col ++ opToStr op ++ valToStr val
    where opToStr op = case op of
            OpEQ -> "="
            OpGT -> ">"
            OpLT -> "<"
          valToStr val = case val of
            VInt i -> show i
            VString s -> "\"" ++ s ++ "\"" -- TODO sql injection prone
            VForeignKey (ForeignKey table column) -> table ++ "." ++ column
