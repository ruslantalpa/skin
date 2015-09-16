module Skin.Types
( Operator (..)
, Value (..)

, ApiRequest
, Field
, RequestNode (..)
, Filter (..)
, Path

--, ForeignKey (..)
, Column (..)
, Table (..)
, Relation (..)
, Condition (..)
, Query (..)
, DbRequest
--, RelationEntry
) where

import Data.Tree
--import Data.Text

data Operator = OpEQ | OpGT | OpLT deriving (Show, Eq)
data Value = VInt Int | VString String | VForeignKey Relation deriving (Show, Eq)

-- Request Types
type ApiRequest = Tree RequestNode
type Field = String
type Path = [String]
data RequestNode = RequestNode {nodeName::String, fields::[Field], filters::[Filter]} deriving (Show, Eq)
data Filter = Filter {field::Field, operator::Operator, value::Value} deriving (Show, Eq)

-- Db Types
--data ForeignKey = ForeignKey {fkTable::String, fkColumn::String} deriving (Show, Eq)
--data Column = Column {colTable:: String, colName::String, colFk::Maybe ForeignKey} deriving (Show, Eq)
--data Table = Table {tblName::String} deriving (Show)

data Column = Column {
  colSchema :: String
, colTable :: String
, colName :: String
, colPosition :: Int
, colNullable :: Bool
, colType :: String
, colUpdatable :: Bool
, colMaxLen :: Maybe Int
, colPrecision :: Maybe Int
, colDefault :: Maybe String
, colEnum :: [String]
--, colFK :: Maybe ForeignKey
} | Star {colSchema :: String, colTable :: String } deriving (Show, Eq)

data Table = Table {
  tableSchema :: String
, tableName :: String
, tableInsertable :: Bool
} deriving (Show)

-- data ForeignKey = ForeignKey {
--   fkTable :: String
-- , fkCol :: String
-- } deriving (Eq, Show)

--data Relation = Parent Column | Child Column | Many Column Column | Root deriving (Show, Eq)
--type RelationEntry = (String, String, String, Relation)
data Relation = Relation {
  relSchema :: String
, relTable :: String
, relColumn :: String
, relFTable :: String
, relFColumn :: String
, relType :: String
} deriving (Show, Eq)

data Condition = Condition {conColumn::Column, conOperator::Operator, conValue::Value} deriving (Show)
data Query = Select {qMainTable::Table, qSelect::[Column], qJoinTables::[Table], qWhere::[Condition], qRelation::Maybe Relation} deriving (Show)
type DbRequest = Tree Query
