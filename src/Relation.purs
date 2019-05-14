module Relation where

import Data.Function ((#))
import Data.Maybe (Maybe(..))
import Data.Ord ((>))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.Row (class Nub, class Lacks)
import Type.Proxy (Proxy(..))
import Type.Row (class Cons, class Union, RProxy(..))

-- Fields ----------------------

data Field (s :: Symbol) (t :: Type) = Field

fieldName :: forall s t. IsSymbol s => Field s t -> String
fieldName _ = reflectSymbol (SProxy :: _ s)

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
empty :: Relation ()
empty = relation

-- | Create a relation from an array of records
fromRecords :: forall r
  . Array (Record r)
  -> Relation r
fromRecords _ = relation

-- | Select some columns from a relation
-- TODO: Instead of RProxy, we should use List SProxy/Field.
project :: forall r1 r r2
  . Union r1 r2 r
  => RProxy r1
  -> Relation r
  -> Relation r1
project _ _ = relation

-- | Remove some columns from a relation
-- TODO: Instead of RProxy, we should use List SProxy/Field.
except :: forall r1 r r2
  . Union r1 r2 r
  => Nub r r
  => RProxy r1
  -> Relation r
  -> Relation r2
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

-- | Add a new field to a relation. The initial values for the column are computed per record.
addField :: forall s t r1 r2
  . IsSymbol s
  => Cons s t r1 r2
  => SProxy s
  -> (Record r1 -> t)
  -> Relation r1
  -> Relation r2
addField _ _ _ = relation

-- | Remove a single field from a relation
removeField :: forall s t r1 r2
  . IsSymbol s
  => Cons s t r1 r2
  => SProxy s
  -> Relation r2
  -> Relation r1
removeField _ _ = relation


-- Row Ops ------------------------------------------

filter :: forall r1
  . (Record r1 -> Boolean)
  -> Relation r1
  -> Relation r1
filter _ r = r

limit :: forall r
  . Int
  -> Relation r
  -> Relation r
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

-- Aggregations --------------------------------------

-- Aggregation
data Aggregation (from :: #Type) (to :: #Type) = Aggregation

aggCons :: forall f1 f2 f t1 t2 t
  . Union f1 f2 f
  => Union t1 t2 t
  => Aggregation f1 t1
  -> Aggregation f2 t2
  -> Aggregation f t
aggCons _ _ = Aggregation

infixr 8 aggCons as &&=

-- TODO: Generalise to `Num a`
-- TODO: How do I create a singleton row from a symbol?
sumAgg :: forall sIn sOut tIn tOut
  . IsSymbol sIn
  => IsSymbol sOut
  => Cons sIn Int () tIn
  => Cons sOut Int () tOut
  => SProxy sIn
  -> SProxy sOut
  -> Aggregation tIn tOut
sumAgg _ _ = Aggregation

countAgg :: forall sOut tOut
  . IsSymbol sOut
  => Cons sOut Int () tOut
  => SProxy sOut
  -> Aggregation () tOut
countAgg _ = Aggregation

aggregate :: forall from input input' output
  . Union input from input'
  => Nub input' input
  => Aggregation from output
  -> Relation input
  -> Relation output
aggregate _ _ = relation

-- Group By ------------------------------------------

-- TODO

-- There are many different types of group by clauses
--  Group by
--  Group by Grouping sets
--  Group by Cube
--  Group by Rollup
-- probably more?

-- Vanilla group by is treated as an aggregation
groupBy_ :: forall r . RProxy r -> Aggregation r r
groupBy_ _ = Aggregation

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
      # renameField (SProxy :: _ "managerName") (SProxy :: _ "name")
      # join (RProxy :: _ (name :: String)) personTable
      # filter (\r -> r.age > 60)
      # project (RProxy :: _ (name :: String, age :: Int))

-- Number of old managers, and sum of their ages, grouped by manager names (doesn't make much sense)
sumAgesOldManagers :: Relation (numOldManagers :: Int, sumAgesOldManagers :: Int, name :: String)
sumAgesOldManagers =
  oldManagers
    # aggregate
        (sumAgg (SProxy :: _"age") (SProxy :: _"sumAgesOldManagers")
        &&= countAgg (SProxy :: _"numOldManagers")
        &&= groupBy_ (RProxy :: _(name :: String)))

-- TODO: Get ages of all old managers with the specified names
-- Needs us to convert a list to SQL (so that we can do name IN list)
-- nameOldManagersAge :: Array String -> Relation (age :: Int)
-- nameOldManagersAge names = foldr
--   oldManagers
--     # filter (\r -> r.name == "John")
--     # except (RProxy :: _(name :: String))
