{-# LANGUAGE OverloadedStrings #-}
module Skin.Functions
( dbRequestToQuery
, requestNodeToQuery
, addJoinConditions
--, buildRelations
, addRelations
--, addField
--, buildRequest
) where

--import qualified Text.ParserCombinators.Parsec as P hiding ((<|>), many)
import Skin.Types
--import Skin.Parsers
import Data.Tree
import Data.Foldable

import Control.Applicative
import Data.List (intercalate)
--import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.Monoid
--import qualified Data.Text as T
import Control.Error.Util
import Control.Error
import Control.Monad.Trans.Error() -- used only for Alternative instance for Either String


findColumn :: [Column] -> String -> String -> String -> Either String Column
findColumn allColumns s t c = note ("no such column: "++t++"."++c)
                            $ find (\ col -> colSchema col == s && colTable col == t && colName col == c ) allColumns

findTable :: [Table] -> String -> String -> Either String Table
findTable allTables s t = note ("no such table: "++t)
                        $ find (\tb-> s == tableSchema tb && t == tableName tb ) allTables

requestNodeToQuery ::String -> [Table] -> [Column] -> RequestNode -> Either String Query
requestNodeToQuery schema allTables allColumns (RequestNode tblName flds fltrs) =
    Select <$> mainTable <*> select <*> joinTables <*> qwhere <*> rel
    where
        mainTable = findTable allTables schema tblName
        select = mapM (liftA2 (<|>) star col) flds --besides specific columns, we allow * here also
            where
                col = findColumn allColumns schema tblName
                star = isStar schema tblName -- it's ok not to check that the table exists here, mainTable will do the checking
                isStar :: String -> String -> String -> Either String Column
                isStar s t f = if f == "*" then Right $ Star {colSchema = s, colTable = t} else Left "not a star"
        qwhere = mapM (filterToCondition schema allColumns tblName) fltrs
        joinTables = pure []
        rel = pure Nothing

filterToCondition :: String -> [Column] -> String -> Filter -> Either String Condition
filterToCondition schema allColumns table (Filter fld op val) =
    Condition <$> column <*> pure op <*> pure val
    where column = findColumn allColumns schema table fld

addRelations :: [Relation] -> Maybe DbRequest -> DbRequest -> DbRequest
addRelations allRelations parentNode node@(Node query@(Select {qMainTable=table}) forest) =
    case parentNode of
        Nothing -> Node query{qRelation=Nothing} updatedForest
        (Just Node{rootLabel=Select{qMainTable=parentTable}}) -> Node query{qRelation=rel} updatedForest
            where
                rel = findRelation allRelations (tableSchema table) (tableName table) (tableName parentTable)
                findRelation :: [Relation] -> String -> String -> String -> Maybe Relation
                findRelation relations schema t1 t2 = find (\(Relation{relSchema=s, relTable=t, relFTable=ft})->t1==t&&t2==ft&&schema==s) relations
    where
        updatedForest = map (addRelations allRelations (Just node)) forest

addJoinConditions :: [Column] -> Tree Query -> Either String DbRequest
addJoinConditions allColumns (Node query@(Select{qJoinTables=from, qWhere=conditions, qRelation=relation}) forest) =
    case relation of
        --Nothing -> Left "no relation between nodes" -- blow up
        Nothing -> Node <$> pure updatedQuery <*> updatedForest
        Just rel@(Relation{relType="child"}) -> Node <$> (addCond updatedQuery <$> getJoinCondition rel) <*> updatedForest
            where addCond q con = q{qWhere=con:qWhere q}
        Just (Relation{relType="parent"}) -> Node <$> pure updatedQuery <*> updatedForest
        _ -> undefined
        -- Just (Many relationColumn1 relationColumn2) -> Node <$> pure updatedQuery{qJoinTables=linkTable:qJoinTables updatedQuery, qWhere=cond1:cond2:qWhere updatedQuery} <*> updatedForest
            -- where
            --     cond1 = getJoinCondition relationColumn1
            --     cond2 = getJoinCondition relationColumn2
            --     linkTable = Table "public" (colTable relationColumn1) True
    where
        updatedQuery = query {qWhere=parentJoinConditions++conditions, qJoinTables=from++parentTables}
            where
                parentJoinConditions = rights $ map (getJoinCondition.snd) parents
                parentTables = map fst parents
                parents = mapMaybe (getParents.rootLabel) forest
                getParents q@(Select{qRelation=(Just rel@(Relation{relType="parent"}))}) = Just (qMainTable q, rel)
                getParents _ = Nothing
        updatedForest = mapM (addJoinConditions allColumns) forest
        getJoinCondition rel@(Relation s t c _ _ _) = Condition <$> col <*> pure OpEQ <*> pure (VForeignKey rel)
            where col = findColumn allColumns s t c

dbRequestToQuery :: DbRequest -> String
dbRequestToQuery (Node (Select mainTable columns tables conditions relation) forest) =
    case relation of
        Nothing -> "SELECT "
                  <> "pg_catalog.count(t),"
                  <> "array_to_json(array_agg(row_to_json(t)))::CHARACTER VARYING AS json "
                  <> "FROM ("
                  <> query
                  <> ") t;"

        _         -> query
    where
        query = unwords [
            ("WITH " <> intercalate ", " withs) `emptyOnNull` withs,
            "SELECT ", intercalate ", " (map colToStr columns ++ selects),
            "FROM ", intercalate ", " (map tblToStr (mainTable:tables)),
            ("WHERE " <> intercalate " AND " ( map conditionToStr conditions )) `emptyOnNull` conditions
            ]
        emptyOnNull val x = if null x then "" else val
        (withs, selects) = foldr getQueryParts ([],[]) forest
        --getQueryParts is not total but dbRequestToQuery is called only after addJoinConditions which ensures the only
        --posible relations are Root Child Parent Many
        getQueryParts (Node (Select _ _ _ _ Nothing) _) _ = undefined
        getQueryParts (Node q@(Select{qMainTable=table, qRelation=(Just (Relation {relType="child"}))}) forst) (w,s) = (w,sel:s)
            where name = tableName table
                  sel = "("
                     <> "SELECT array_to_json(array_agg(row_to_json("<>name<>"))) "
                     <> "FROM (" <> dbRequestToQuery (Node q forst) <> ") " <> name
                     <> ") AS " <> name
        getQueryParts (Node q@(Select{qMainTable=table, qRelation=(Just (Relation{relType="parent"}))}) forst) (w,s) = (wit:w,sel:s)
            where name = tableName table
                  sel = "row_to_json(" <> name <> ".*) AS "<>name --TODO must be singular
                  wit = name <> " AS ( " <> dbRequestToQuery (Node q forst) <> " )"
        -- getQueryParts (Node q@(Select{qMainTable=table, qRelation=(Just (Many _ _))}) forst) (w,s) = (w,sel:s)
        --     where name = tableName table
        --           sel = "("
        --              <> "SELECT array_to_json(array_agg(row_to_json("<>name<>"))) "
        --              <> "FROM (" <> dbRequestToQuery (Node q forst) <> ") " <> name
        --              <> ") AS " <> name

colToStr :: Column -> String
colToStr Column {colSchema=s, colTable=t, colName=c} = "\"" <> s <> "\"." <> t <> "." <> c
colToStr Star {colSchema=s, colTable=t} = "\"" <> s <> "\"." <> t <> ".*"

tblToStr :: Table -> String
tblToStr tbl = "\"" <> tableSchema tbl <> "\"." <> tableName tbl

conditionToStr :: Condition -> String
conditionToStr (Condition col op val) = colToStr col <> opToStr op <> valToStr val
    where opToStr o = case o of
            OpEQ -> "="
            OpGT -> ">"
            OpLT -> "<"
          valToStr v = case v of
            VInt i -> show i
            VString s -> "'" <> s <> "'" -- TODO sql injection prone
            VForeignKey (Relation{relFTable=table, relFColumn=column}) -> table <> "." <> column
