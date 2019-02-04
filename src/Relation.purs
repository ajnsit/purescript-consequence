module Relation where

import Data.Function ((#))
import Data.Maybe (Maybe(..))
import Data.Ord ((>))
import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row (class Nub, class Lacks)
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
project :: forall r1 r r2. Union r1 r2 r => RProxy r1 -> Relation r -> Relation r1
project _ _ = relation

-- | Remove some columns from a relation
-- TODO: Instead of RProxy, we should use List SProxy/Field.
except :: forall r1 r r2. Union r1 r2 r => Nub r r => RProxy r1 -> Relation r -> Relation r2
except _ _ = relation

-- | Rename a field in a relation
renameField :: forall prev next ty input inter output
   . IsSymbol prev
  => IsSymbol next
  => Cons prev ty inter input
  => Lacks prev inter
  => Cons next ty inter output
  => Lacks next inter
  => SProxy prev
  -> SProxy next
  -> Relation input
  -> Relation output
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


join
  :: forall join a relation1 b relation2 result
   . Union join a relation1
  => Union join b relation2
  => Union a relation2 result
  => RProxy join
  -> Relation relation1
  -> Relation relation2
  -> Relation result
join _ _ _ = relation

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
oldManagers :: PersonRel
oldManagers =
    employeeTable
      # renameField (SProxy :: SProxy "managerName") (SProxy :: SProxy "name")
      # join (RProxy :: RProxy (name :: String)) personTable
      # filter (\r -> r.age > 60)
      # project (RProxy :: RProxy (name :: String, age :: Int))
