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
import           Skin.Types
--import Skin.Parsers
import           Data.Foldable
import           Data.Tree

import           Control.Applicative
import           Data.List           (intercalate)
--import qualified Data.ByteString.Char8 as B
import           Data.Maybe
import           Data.Monoid
--import qualified Data.Text as T
import           Control.Error
--import           Control.Error.Util
--import           Control.Monad.Trans.Error ()


-- TODO use Error.Util to turn either to maybe then we can use <|> without Trans

findColumn :: [Column] -> String -> String -> String -> Either String Column
findColumn allColumns s t c = note ("no such column: "++t++"."++c) $
    find (\ col -> colSchema col == s && colTable col == t && colName col == c ) allColumns

findTable :: [Table] -> String -> String -> Either String Table
findTable allTables s t = note ("no such table: "++t) $
    find (\tb-> s == tableSchema tb && t == tableName tb ) allTables

findRelation :: [Relation] -> String -> String -> String -> Maybe Relation
findRelation allRelations s t1 t2 =
    find (\r -> s == relSchema r && t1 == relTable r && t2 == relFTable r) allRelations

requestNodeToQuery ::String -> [Table] -> [Column] -> RequestNode -> Either String Query
requestNodeToQuery schema allTables allColumns (RequestNode tblName flds fltrs) =
    Select <$> mainTable <*> select <*> joinTables <*> qwhere <*> rel
    where
        mainTable = findTable allTables schema tblName
        select = mapM toColumn flds --besides specific columns, we allow * here also
            where
                toColumn = note ("no such column: "++tblName++".!!! name missing") . liftA2 (<|>) matchStar matchColumn
                matchColumn = hush . findColumn allColumns schema tblName
                matchStar = hush . isStar schema tblName -- it's ok not to check that the table exists here, mainTable will do the checking
                isStar :: String -> String -> String -> Either String Column
                isStar s t "*" = Right $ Star {colSchema = s, colTable = t}
                isStar _ _ _   = Left "not a star"
        qwhere = mapM (filterToCondition schema allColumns tblName) fltrs
        joinTables = pure []
        rel = pure Nothing

filterToCondition :: String -> [Column] -> String -> Filter -> Either String Condition
filterToCondition schema allColumns table (Filter fld op val) =
    Condition <$> column <*> pure op <*> pure val
    where column = findColumn allColumns schema table fld

addRelations :: [Relation] -> Maybe DbRequest -> DbRequest -> Either String DbRequest
addRelations allRelations parentNode node@(Node query@(Select {qMainTable=table}) forest) =
    case parentNode of
        Nothing -> Node query{qRelation=Nothing} <$> updatedForest
        (Just (Node (Select{qMainTable=parentTable}) _)) -> Node <$> (addRel query <$> rel) <*> updatedForest
            where
                rel = note ("no relation between " ++ (tableName table) ++ " and " ++ (tableName parentTable)) $
                    findRelation allRelations (tableSchema table) (tableName table) (tableName parentTable)
                addRel :: Query -> Relation -> Query
                addRel q r = q{qRelation = Just r}
    where
        updatedForest = mapM (addRelations allRelations (Just node)) forest

addJoinConditions :: [Column] -> Tree Query -> Either String DbRequest
addJoinConditions allColumns (Node query@(Select{qRelation=relation}) forest) =
    case relation of
        Nothing -> Node <$> updatedQuery <*> updatedForest -- this is the root node
        Just rel@(Relation{relType="child"}) -> Node <$> (addCond <$> updatedQuery <*> getJoinCondition rel) <*> updatedForest
        Just (Relation{relType="parent"}) -> Node <$> updatedQuery <*> updatedForest
        -- Just (Many relationColumn1 relationColumn2) -> Node <$> pure updatedQuery{qJoinTables=linkTable:qJoinTables updatedQuery, qWhere=cond1:cond2:qWhere updatedQuery} <*> updatedForest
            -- where
            --     cond1 = getJoinCondition relationColumn1
            --     cond2 = getJoinCondition relationColumn2
            --     linkTable = Table "public" (colTable relationColumn1) True
        _ -> Left "unknow relation"
    where
        -- add parentTable and parentJoinConditions to the query
        updatedQuery = foldr (flip addCond) (query{qJoinTables = parentTables ++ (qJoinTables query)}) <$> parentJoinConditions
            where
                parentJoinConditions = mapM (getJoinCondition.snd) parents
                parentTables = map fst parents
                parents = mapMaybe (getParents.rootLabel) forest
                getParents qq@(Select{qRelation=(Just rel@(Relation{relType="parent"}))}) = Just (qMainTable qq, rel)
                getParents _ = Nothing
        updatedForest = mapM (addJoinConditions allColumns) forest
        getJoinCondition rel@(Relation s t c _ _ _) = Condition <$> col <*> pure OpEQ <*> pure (VForeignKey rel)
            where col = findColumn allColumns s t c
        addCond q con = q{qWhere=con:qWhere q}

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
        --posible relations are Child Parent Many
        getQueryParts :: Tree Query -> ([String], [String]) -> ([String], [String])
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
        -- the following is just to remove the warning, maybe relType should not be String?
        getQueryParts (Node (Select{qRelation=Nothing}) _) _ = undefined
        getQueryParts (Node (Select{qRelation=(Just (Relation {relType=_}))}) _) _ = undefined

colToStr :: Column -> String
colToStr Column {colSchema=s, colTable=t, colName=c} = "\"" <> s <> "\"." <> t <> "." <> c
colToStr Star {colSchema=s, colTable=t} = "\"" <> s <> "\"." <> t <> ".*"

tblToStr :: Table -> String
tblToStr Table{tableSchema=s, tableName=n} = "\"" <> s <> "\"." <> n

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
