{-# LANGUAGE OverloadedStrings #-}
module Skin.Functions
( dbRequestToQuery
, requestNodeToQuery
, addJoinConditions
, buildRelations
, addRelations
, addField
, buildRequest
) where


import Skin.Types
import Data.Tree
import GHC.Exts (groupWith)
import Control.Applicative
import Data.List (intercalate, intersperse, nub, find, delete)
--import Network.URI
import qualified Data.ByteString.Char8 as B
import Data.Maybe
--import qualified Network.HTTP.Types.URI as URI
import qualified Data.Text as T

buildRequest :: String -> String -> [(String, String)] -> Request
buildRequest rootTable includeStr whereFilters =
    foldr insertFilter (foldr insertField rootNode splitedInclude) conditionEntries
    where
        rootNode = Node (RequestNode rootTable [] []) []
        splitedInclude = map (T.splitOn ".") $ map T.strip $ T.splitOn "," $ T.pack includeStr
        conditionEntries = map toConditionEntry whereFilters
        toConditionEntry (fldPath, opAndValue) = ((path, field), strToOp op, strToVal val)
            where
                op:rest = map T.unpack $ T.split (=='.') $ T.pack opAndValue
                val = intercalate "." rest
                item = map T.unpack $ T.splitOn "." $ T.pack fldPath
                path = init item
                field = last item
                strToOp str = case str of
                         "eq" -> OpEQ
                         "gt" -> OpGT
                         "lt" -> OpLT
                strToVal str =
                    if length readint == 1 && "" == ris
                    then VInt i
                    else VString str
                    where
                        readint = reads str :: [(Int, String)]
                        (i, ris) = head readint
        insertField item node = addField node path field
            where
                path = map T.unpack $ init item
                field = T.unpack $ last item
        insertFilter ((path, field), op, val) node = addFilter node path (Filter field op val)

addField :: Request -> Path -> Field -> Request
addField (Node rn@(RequestNode {fields=flds}) forest) [] field = Node (rn {fields=field:flds}) forest
addField (Node rn forest) path field = Node rn (addField targetNode remainingPath field:restForest)
    where targetNodeName:remainingPath = path
          (targetNode,restForest) = splitForest targetNodeName forest
          splitForest name forest =
              case maybeNode of
                  Nothing -> (Node (RequestNode name [] []) [],forest)
                  Just node -> (node, delete node forest)
              where maybeNode = find ((name==).nodeName.rootLabel) forest

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
    Query <$> mainTable <*> select <*> from <*> qwhere <*> rel
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
addRelations allRelations parentNode node@(Node query@(Query {qMainTable=table}) forest) =
    case parentNode of
        Nothing -> Node query {qRelation=Just Root} updatedForest
        (Just Node{rootLabel=Query{qMainTable=parentTable}}) -> Node query {qRelation=rel} updatedForest
            where
                rel = findRelation allRelations (tblName table) (tblName parentTable)
    where
        updatedForest = map (addRelations allRelations (Just node)) forest
        findRelation :: [RelationEntry] -> String -> String -> Maybe Relation
        findRelation relations t1 t2 = getRelation <$> find (\(f,s,rs,r)->t1==f&&t2==s) relations
            where getRelation (_,_,_,r) = r

buildRelations :: [Column] -> [RelationEntry]
buildRelations columns = sRel ++ mRel
    where sRel = simpleRelations columns
          mRel = linkRelations sRel

          linkRelations :: [RelationEntry] -> [RelationEntry]
          linkRelations relations = concatMap link2Relation links
                where links = filter (\g ->length g == 2 && all (\(f,s,rs,r)->rs=="child") g) $ groupWith (\(f,s,rs,r)->f) relations
                      link2Relation link = [(t1,t2,"many", Many r1 r2),(t2,t1,"many", Many r1 r2)]
                          where linktbl = head $ map (\(f,s,rs,r)->f) link
                                [(t1,Child r1),(t2,Child r2)] = map (\(f,s,rs,r)->(s,r)) link

          simpleRelations :: [Column]->[RelationEntry]
          simpleRelations = concatMap (\column@(Column {colTable=table, colFk=Just (ForeignKey {fkTable=fTable})})->[(table, fTable, "child", Child column),(fTable, table, "parent", Parent column)])
                            . filter (isJust.colFk)

addJoinConditions :: Tree Query -> Maybe (Tree Query)
addJoinConditions (Node query@(Query{qMainTable=table, qFrom=from, qWhere=conditions, qRelation=relation}) forest) =
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
        getParents q@(Query{qRelation=rel@(Just (Parent relationColumn))}) = Just (qMainTable q, relationColumn)
        getParents _ = Nothing
        getJoinCondition relationColumn = Condition relationColumn OpEQ (VForeignKey ((fromJust.colFk) relationColumn))

dbRequestToQuery :: DbRequest -> String
dbRequestToQuery (Node (Query mainTable columns tables conditions relation) forest) =
    unwords [
        withsStr,
        "\nSELECT", intercalate ", " (map colToStr columns ++ selects),
        "\nFROM", intercalate ", " (tblName mainTable:map tblName tables),
        --"\nWHERE", intercalate " AND " ("1=1":map conditionToStr conditions)
        whereStr
    ]
    where withsStr = if null withs then "" else "WITH " ++ intercalate ", " withs
          whereStr = if null conditions then "" else "WHERE " ++ intercalate " AND " ( map conditionToStr conditions )
          (withs, selects) = foldr getQueryParts ([],[]) forest
          getQueryParts (Node query@(Query{qMainTable=table, qRelation=(Just (Child _))}) forest) (w,s) = (w,sel:s)
            where name = tblName table
                  sel = "(SELECT array_to_json(array_agg(row_to_json("++name++"))) FROM (" ++ dbRequestToQuery (Node query forest) ++ ") "++name++" ) AS "++name
          getQueryParts (Node query@(Query{qMainTable=table, qRelation=(Just (Parent _))}) forest) (w,s) = (wit:w,sel:s)
            where name = tblName table
                  sel = "row_to_json("++name++".*) AS "++name --TODO must be singular
                  wit = name ++ " AS ( " ++ dbRequestToQuery (Node query forest) ++ " )"
          getQueryParts (Node query@(Query{qMainTable=table, qRelation=(Just (Many _ _))}) forest) (w,s) = (w,sel:s)
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
            VString s -> s
            VForeignKey (ForeignKey table column) -> table ++ "." ++ column
