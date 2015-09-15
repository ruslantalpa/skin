{-# LANGUAGE QuasiQuotes, OverloadedStrings, TypeSynonymInstances,
             MultiParamTypeClasses, ScopedTypeVariables,
             FlexibleContexts #-}

module Skin.Structure
(
--   getColumns
-- , getTables
  columns
, tables
, relations
--, buildRelations
)
where

import qualified Hasql as H
import qualified Hasql.Postgres as P
--import qualified Data.Map as Map
--import Data.String.Conversions (cs)
import Skin.Types
import Data.Maybe
--import GHC.Exts (groupWith)
import Data.Text (Text, unpack, split) -- hiding (foldl, map, zipWith, concat, concatMap, filter, length, head, all)




tableFromRow :: (Text, Text, Bool) -> Table
tableFromRow (s, n, i) = Table (unpack s) (unpack n) i

columnFromRow :: (Text,       Text,      Text,
                  Int,        Bool,      Text,
                  Bool,       Maybe Int, Maybe Int,
                  Maybe Text, Maybe Text)
              -> Column
columnFromRow (s, t, n, pos, nul, typ, u, l, p, d, e) =
  Column (unpack s) (unpack t) (unpack n) pos nul (unpack typ) u l p (unpack <$> d) (parseEnum e)
  where
    parseEnum :: Maybe Text -> [String]
    parseEnum str = fromMaybe [] $ (map unpack . split (==',')) <$> str

relationFromRow :: (Text, Text, Text, Text, Text) -> Relation
relationFromRow (s, t, c, ft, fc) = Relation (unpack s) (unpack t) (unpack c) (unpack ft) (unpack fc) "child"

tables :: H.Tx P.Postgres s [Table]
tables = do
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
          and n.nspname not in ('information_schema', 'pg_catalog')
          and (
            pg_has_role(c.relowner, 'USAGE'::text)
            or has_table_privilege(c.oid, 'SELECT, INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER'::text)
            or has_any_column_privilege(c.oid, 'SELECT, INSERT, UPDATE, REFERENCES'::text)
          )
        order by relname
      |]
    return $ map tableFromRow rows

relations :: H.Tx P.Postgres s [Relation]
relations = do
    rels <- H.listEx $ [H.stmt|
        SELECT DISTINCT tc.table_schema, tc.table_name, kcu.column_name, ccu.table_name AS foreign_table_name, ccu.column_name AS foreign_column_name
        FROM information_schema.table_constraints AS tc
        JOIN information_schema.key_column_usage AS kcu on tc.constraint_name = kcu.constraint_name
        JOIN information_schema.constraint_column_usage AS ccu on ccu.constraint_name = tc.constraint_name
        WHERE constraint_type = 'FOREIGN KEY'
        AND tc.table_schema NOT IN ('pg_catalog', 'information_schema')
        ORDER BY tc.table_schema, tc.table_name, kcu.column_name
    |]
    return $ foldr addFlippedRelation [] $ map relationFromRow rels
    where
        addFlippedRelation rel@(Relation s t c ft fc r) relations = (Relation s ft fc t c "parent"):rel:relations

columns :: H.Tx P.Postgres s [Column]
columns = do
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
           where table_schema not in ('pg_catalog', 'information_schema')
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
      order by schema, position |]

  return $ map columnFromRow cols
  -- fks <- foreignKeys table
  -- return $ map (addFK fks . columnFromRow) cols
  --
  -- where
  --   addFK fks col = col { colFK = Map.lookup (cs .colName $ col) fks }
