module Skin.Types
( Operator (..)
, Value (..)

, ApiRequest
, Field
, Cast
, SelectItem
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
, DbField
, DbSelectItem
, JsonPath
--, RelationEntry
) where

import           Data.Tree
--import Data.Text

data Operator = OpEQ | OpGT | OpLT deriving (Show, Eq)
data Value = VInt Int | VString String | VForeignKey Relation deriving (Show, Eq)

-- Request Types
type ApiRequest = Tree RequestNode
type FieldName = String
type JsonPath = [String]
type Field = (FieldName, Maybe JsonPath)
type Cast = String
type SelectItem = (Field, Maybe Cast)
type Path = [String]
data RequestNode = RequestNode {nodeName::String, fields::[SelectItem], filters::[Filter]} deriving (Show, Eq)
data Filter = Filter {field::Field, operator::Operator, value::Value} deriving (Show, Eq)

-- Db Types
type DbField = (Column, Maybe JsonPath)
type DbSelectItem = (DbField, Maybe Cast)
data Condition = Condition {conColumn::DbField, conOperator::Operator, conValue::Value} deriving (Show)
data Query = Select {qMainTable::Table, qSelect::[DbSelectItem], qJoinTables::[Table], qWhere::[Condition], qRelation::Maybe Relation} deriving (Show)
type DbRequest = Tree Query
data Column = Column {
  colSchema    :: String
, colTable     :: String
, colName      :: String
, colPosition  :: Int
, colNullable  :: Bool
, colType      :: String
, colUpdatable :: Bool
, colMaxLen    :: Maybe Int
, colPrecision :: Maybe Int
, colDefault   :: Maybe String
, colEnum      :: [String]
--, colFK :: Maybe ForeignKey
} | Star {colSchema :: String, colTable :: String } deriving (Show, Eq)

data Table = Table {
  tableSchema     :: String
, tableName       :: String
, tableInsertable :: Bool
} deriving (Show)

-- data ForeignKey = ForeignKey {
--   fkTable :: String
-- , fkCol :: String
-- } deriving (Eq, Show)

data Relation = Relation {
  relSchema  :: String
, relTable   :: String
, relColumn  :: String
, relFTable  :: String
, relFColumn :: String
, relType    :: String
} deriving (Show, Eq)
