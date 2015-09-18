{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

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

import qualified Hasql          as H
import qualified Hasql.Postgres as P
--import qualified Data.Map as Map
--import Data.String.Conversions (cs)
import           Data.Maybe
import           Skin.Types
--import GHC.Exts (groupWith)
import           Data.Text      (Text, split, unpack)

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

addFlippedRelation :: Relation -> [Relation] -> [Relation]
addFlippedRelation rel@(Relation s t c ft fc _) rels = Relation s ft fc t c "parent":rel:rels

tables :: H.Tx P.Postgres s [Table]
tables = do
    rows <- H.listEx $ [H.stmt|
            SELECT  n.nspname AS table_schema,
                    relname   AS TABLE_NAME,
                    c.relkind = 'r' OR (c.relkind IN ('v','f'))
                    AND (pg_relation_is_updatable(c.oid::regclass, FALSE) & 8) = 8
                    OR (EXISTS ( SELECT 1
                                 FROM pg_trigger
                                 WHERE pg_trigger.tgrelid = c.oid
                                 AND (pg_trigger.tgtype::integer & 69) = 69)
                    ) AS insertable
            FROM pg_class c
            JOIN pg_namespace n ON n.oid = c.relnamespace
            WHERE   c.relkind IN ('v','r','m')
                AND n.nspname NOT IN ('information_schema','pg_catalog')
                AND (  pg_has_role(c.relowner, 'USAGE'::text)
                    OR has_table_privilege(c.oid, 'SELECT, INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER'::text)
                    OR has_any_column_privilege(c.oid, 'SELECT, INSERT, UPDATE, REFERENCES'::text)
                    )
            ORDER BY relname
        |]
    return $ map tableFromRow rows

relations :: H.Tx P.Postgres s [Relation]
relations = do
    rels <- H.listEx $ [H.stmt|
        WITH table_fk AS (
            SELECT DISTINCT
                tc.table_schema, tc.table_name, kcu.column_name,
                ccu.table_name AS foreign_table_name,
                ccu.column_name AS foreign_column_name
            FROM information_schema.table_constraints AS tc
            JOIN information_schema.key_column_usage AS kcu on tc.constraint_name = kcu.constraint_name
            JOIN information_schema.constraint_column_usage AS ccu on ccu.constraint_name = tc.constraint_name
            WHERE   constraint_type = 'FOREIGN KEY'
                AND tc.table_schema NOT IN ('pg_catalog', 'information_schema')
            ORDER BY tc.table_schema, tc.table_name, kcu.column_name
        )
        SELECT * FROM table_fk
        UNION
        (
            SELECT DISTINCT
                vcu.table_schema, vcu.view_name AS table_name, vcu.column_name,
                table_fk.foreign_table_name,
                table_fk.foreign_column_name
            FROM information_schema.view_column_usage as vcu
            JOIN table_fk ON
                table_fk.table_schema = vcu.view_schema AND
                table_fk.table_name = vcu.table_name AND
                table_fk.column_name = vcu.column_name
            WHERE vcu.view_schema NOT IN ('pg_catalog', 'information_schema')
            ORDER BY vcu.table_schema, vcu.view_name, vcu.column_name
        )

    |]
    return $ foldr (addFlippedRelation.relationFromRow) [] rels

columns :: H.Tx P.Postgres s [Column]
columns = do
    cols <- H.listEx $ [H.stmt|
        SELECT
            info.table_schema AS schema,
            info.table_name AS table_name,
            info.column_name AS name,
            info.ordinal_position AS position,
            info.is_nullable::boolean AS nullable,
            info.data_type AS col_type,
            info.is_updatable::boolean AS updatable,
            info.character_maximum_length AS max_len,
            info.numeric_precision AS precision,
            info.column_default AS default_value,
            array_to_string(enum_info.vals, ',') AS enum
        FROM (
            SELECT
                table_schema,
                table_name,
                column_name,
                ordinal_position,
                is_nullable,
                data_type,
                is_updatable,
                character_maximum_length,
                numeric_precision,
                column_default,
                udt_name
            FROM information_schema.columns
            WHERE table_schema NOT IN ('pg_catalog', 'information_schema')
        ) AS info
        LEFT OUTER JOIN (
            SELECT
                n.nspname AS s,
                t.typname AS n,
                array_agg(e.enumlabel ORDER BY e.enumsortorder) AS vals
            FROM pg_type t
            JOIN pg_enum e ON t.oid = e.enumtypid
            JOIN pg_catalog.pg_namespace n ON n.oid = t.typnamespace
            GROUP BY s,n
        ) AS enum_info ON (info.udt_name = enum_info.n)
        ORDER BY schema, position
    |]
    return $ map columnFromRow cols
  -- fks <- foreignKeys table
  -- return $ map (addFK fks . columnFromRow) cols
  --
  -- where
  --   addFK fks col = col { colFK = Map.lookup (cs .colName $ col) fks }
