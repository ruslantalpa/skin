module Skin.Types
( Operator (..)
, Value (..)

, Request
, Field
, RequestNode (..)
, Filter (..)
, Path

, ForeignKey (..)
, Column (..)
, Table (..)
, Relation (..)
, Condition (..)
, Query (..)
, DbRequest
, RelationEntry
) where

import Data.Tree

data Operator = OpEQ | OpGT | OpLT deriving (Show, Eq)
data Value = VInt Int | VString String | VForeignKey ForeignKey deriving (Show, Eq)

-- Request Types
type Request = Tree RequestNode
type Field = String
type Path = [String]
data RequestNode = RequestNode {nodeName::String, fields::[Field], filters::[Filter]} deriving (Show, Eq)
data Filter = Filter {field::Field, operator::Operator, value::Value} deriving (Show, Eq)

-- Db Types
data ForeignKey = ForeignKey {fkTable::String, fkColumn::String} deriving (Show, Eq)
data Column = Column {colTable:: String, colName::String, colFk::Maybe ForeignKey} deriving (Show, Eq)
data Table = Table {tblName::String} deriving (Show)
data Relation = Parent Column | Child Column | Many Column Column | Root deriving (Show, Eq) --trebuie sa contina toate datele pentru a face legatura
type RelationEntry = (String, String, String, Relation)
data Condition = Condition {conColumn::Column, conOperator::Operator, conValue::Value} deriving (Show)
data Query = Select {qMainTable::Table, qSelect::[Column], qFrom::[Table], qWhere::[Condition], qRelation::Maybe Relation} deriving (Show)
type DbRequest = Tree Query
