{-# LANGUAGE QuasiQuotes, OverloadedStrings, TypeSynonymInstances,
             MultiParamTypeClasses, ScopedTypeVariables,
             FlexibleContexts #-}

module Skin.Structure
(
--   getColumns
-- , getTables
  columns
, tables
, buildRelations
)
where

import qualified Hasql as H
import qualified Hasql.Postgres as P
import qualified Data.Map as Map
import Data.String.Conversions (cs)
import Skin.Types
import Data.Maybe
import GHC.Exts (groupWith)
import Data.Text hiding (foldl, map, zipWith, concat, concatMap, filter, length, head, all)


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

tableFromRow :: (Text, Text, Bool) -> Table
--tableFromRow (s, n, i) = Table s n i
tableFromRow (s, n, i) = Table $ unpack n

columnFromRow :: (Text,       Text,      Text,
                  Int,        Bool,      Text,
                  Bool,       Maybe Int, Maybe Int,
                  Maybe Text, Maybe Text)
              -> Column
columnFromRow (s, t, n, pos, nul, typ, u, l, p, d, e) =
  --Column s t n pos nul typ u l p d (parseEnum e) Nothing
  Column (unpack t) (unpack n) Nothing
  -- where
  --   parseEnum :: Maybe Text -> [Text]
  --   parseEnum str = fromMaybe [] $ split (==',') <$> str

tables :: H.Tx P.Postgres s [Table]
tables = do
    let schema = "public"::Text
    rows <- H.listEx $
      [H.stmt|
        select
          n.nspname as table_schema,
          relname as table_name,
          c.relkind = 'r' or (c.relkind IN ('v', 'f')) and (pg_relation_is_updatable(c.oid::regclass, false) & 8) = 8
          or (exists (
             select 1
             from pg_trigger
             where pg_trigger.tgrelid = c.oid and (pg_trigger.tgtype::integer & 69) = 69)
          ) as insertable
        from
          pg_class c
          join pg_namespace n on n.oid = c.relnamespace
        where
          c.relkind in ('v', 'r', 'm')
          and n.nspname = ?
          and (
            pg_has_role(c.relowner, 'USAGE'::text)
            or has_table_privilege(c.oid, 'SELECT, INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER'::text)
            or has_any_column_privilege(c.oid, 'SELECT, INSERT, UPDATE, REFERENCES'::text)
          )
        order by relname
      |] schema
    return $ map tableFromRow rows

foreignKeys :: Table -> H.Tx P.Postgres s (Map.Map Text ForeignKey)
foreignKeys table = do
  r <- H.listEx $ [H.stmt|
      select kcu.column_name, ccu.table_name AS foreign_table_name,
        ccu.column_name AS foreign_column_name
      from information_schema.table_constraints AS tc
        join information_schema.key_column_usage AS kcu
          on tc.constraint_name = kcu.constraint_name
        join information_schema.constraint_column_usage AS ccu
          on ccu.constraint_name = tc.constraint_name
      where constraint_type = 'FOREIGN KEY'
        and tc.table_name=? and tc.table_schema = ?
        order by kcu.column_name
    |] (pack $ tblName table) ("public"::Text)

  return $ foldl addKey Map.empty r
  where
    addKey :: Map.Map Text ForeignKey -> (Text, Text, Text) -> Map.Map Text ForeignKey
    addKey m (col, ftab, fcol) = Map.insert col (ForeignKey (unpack ftab) (unpack fcol)) m

columns :: Table -> H.Tx P.Postgres s [Column]
columns table = do
  cols <- H.listEx $ [H.stmt|
      select info.table_schema as schema, info.table_name as table_name,
            info.column_name as name, info.ordinal_position as position,
            info.is_nullable::boolean as nullable, info.data_type as col_type,
            info.is_updatable::boolean as updatable,
            info.character_maximum_length as max_len,
            info.numeric_precision as precision,
            info.column_default as default_value,
            array_to_string(enum_info.vals, ',') as enum
        from (
          select table_schema, table_name, column_name, ordinal_position,
                 is_nullable, data_type, is_updatable,
                 character_maximum_length, numeric_precision,
                 column_default, udt_name
            from information_schema.columns
           where table_schema = ? and table_name = ?
        ) as info
        left outer join (
          select n.nspname as s,
                 t.typname as n,
                 array_agg(e.enumlabel ORDER BY e.enumsortorder) as vals
          from pg_type t
            join pg_enum e on t.oid = e.enumtypid
            join pg_catalog.pg_namespace n ON n.oid = t.typnamespace
          group by s, n
        ) as enum_info
        on (info.udt_name = enum_info.n)
      order by position |]
    ("public"::Text) (pack $ tblName table)

  fks <- foreignKeys table
  return $ map (addFK fks . columnFromRow) cols

  where
    addFK fks col = col { colFk = Map.lookup (cs .colName $ col) fks }
