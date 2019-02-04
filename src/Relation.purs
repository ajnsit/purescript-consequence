module Relation where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row (class Nub)
import Type.Proxy (Proxy(..))
import Type.Row (class Cons, class ListToRow, class Union, Nil, RProxy(..))

-- Fields ----------------------

data Field (s :: Symbol) (t :: Type) = Field

fieldName :: forall s t. Field s t -> SProxy s
fieldName _ = SProxy

fieldType :: forall s t. Field s t -> Proxy t
fieldType _ = Proxy

field' :: forall s t. SProxy s -> Field s t
field' _ = Field

field :: forall s t. Field s t
field = Field

data RefField = RefField String

-- Relations ----------------------

type TableName = String

-- Relations can be anonymous or concrete (DB Tables)
data Relation (r :: #Type) = Relation (Maybe TableName)

relationName :: forall r. Relation r -> Maybe TableName
relationName (Relation n) = n

relation' :: forall r. RProxy r -> Relation r
relation' _ = Relation Nothing

relation :: forall r. Relation r
relation = Relation Nothing

-- Operations ------------------------------------

-- Use `relation` to proxy for unimplemented functions

-- Due to complications with duplicate labels, some of these types are pretty long. Could they be simplified?
-- Also consider duplicate labels, but different types. Are those handled by Nub?

-- | An empty relation
-- Do we have a predefined empty row? Going through RowList seems hacky.
empty :: forall x. ListToRow Nil x => Relation x
empty = relation

-- | Select some columns from a relation
-- TODO: Instead of RProxy, we should use List SProxy/Field.
project :: forall r1 r r2. Union r1 r2 r => Nub r r => RProxy r2 -> Relation r -> Relation r2
project _ _ = relation

-- | Remove some columns from a relation
-- TODO: Instead of RProxy, we should use List SProxy/Field.
except :: forall r1 r r2. Union r1 r2 r => Nub r r => RProxy r1 -> Relation r -> Relation r2
except _ _ = relation

-- | Rename a field in a relation
renameField :: forall s1 s2 x r r1 r2. IsSymbol s1 => IsSymbol s2 => Cons s1 x r r1 => Cons s2 x r r2 => SProxy s1 -> SProxy s2 -> Relation r1 -> Relation r2
renameField _ _ _ = relation

-- | Add a new field to a relation
addField :: forall s t r1 r2. IsSymbol s => Cons s t r1 r2 => Field s t -> Relation r1 -> Relation r2
addField _ _ = relation

-- | Remove a single field from a relation
removeField :: forall s t r1 r2. IsSymbol s => Cons s t r1 r2 => SProxy s -> Relation r2 -> Relation r1
removeField _ _ = relation


-- Row Ops ------------------------------------------

filter :: forall r1. (Record r1 -> Boolean) -> Relation r1 -> Relation r1
filter _ r = r

limit :: forall r. Int -> Relation r -> Relation r
limit _ r = r

-- Joins -----------------------------------


join :: forall a b x r1 r2 r. Union a x r1 => Nub r1 r1 => Union b x r2 => Nub r2 r2 => Union r1 b r => Union a r2 r => Nub r r => RProxy x -> Relation r1 -> Relation r2 -> Relation r
join _ _ _ = relation

join' :: forall a b x r1 r2 r r'. Union a x r1 => Nub r1 r1 => Union b x r2 => Nub r2 r2 => Union r1 b r => Union a r2 r => Union r1 r2 r' => Nub r' r => Nub r r => Relation r1 -> Relation r2 -> Relation r
join' _ _ = relation



-- Example -------------------------------------------
nameField :: Field "name" String
nameField = field

ageField :: Field "age" Int
ageField = field

-- Tables
type PersonRel = Relation (name :: String, age :: Int)
type EmployeeRel = Relation (employeeName :: String, managerName :: String)

personTable :: PersonRel
personTable = relation

employeeTable :: EmployeeRel
employeeTable = relation

-- Get all managers older than 60
-- oldManagers :: PersonRel
oldManagers = employeeTable
  # renameField (SProxy :: SProxy "managerName") (SProxy :: SProxy "name")
  # join (RProxy :: RProxy (name :: String)) personTable
  # filter (\r -> r.age > 60)
  # project (RProxy :: RProxy (name :: String, age :: Int))
