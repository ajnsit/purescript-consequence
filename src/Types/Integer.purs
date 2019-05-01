module Types.Integer where

-- Integer operations
import Control.Bind (bind, pure)
import Data.Array (length)
import Data.Boolean (otherwise)
import Data.Either (Either(..))
import Data.Exists (runExists)
import Data.Function ((#), ($))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Ord ((>=))
import Data.Ring ((*), (-))
import Data.Semigroup ((<>))
import Data.Semiring ((+))
import Data.Show (show)
import Deserialiser (DynDeserialiser, ExtensibleDeserialiser, Deserialiser, deserialiseWith, dfix)
import Expr (Expr, ExprU(..), eop0, eop2)
import Pretty (Pretty, sop0, sop2)
import Run (Run, rop0, rop2)
import Typing (Dynamic(..), Dynamic'(..), asInt, mkDynamic, tbool, tint)

-- Integer operations
class IntOps r where
  int  :: forall h. Int -> r h Int
  add  :: forall h. r h Int -> r h Int -> r h Int
  sub  :: forall h. r h Int -> r h Int -> r h Int
  mul  :: forall h. r h Int -> r h Int -> r h Int
  gte  :: forall h. r h Int -> r h Int -> r h Boolean

-- Helpful synonyms
infixr 4 mul as #*
infixr 5 add as #+
infixr 5 sub as #-
infixr 6 gte as #>=

instance intSymRun :: IntOps Run where
  int = rop0
  add = rop2 (+)
  sub = rop2 (-)
  mul = rop2 (*)
  gte = rop2 (>=)

instance intSymPretty :: IntOps Pretty where
  int = sop0
  add = sop2 "+"
  sub = sop2 "-"
  mul = sop2 "*"
  gte = sop2 ">="

instance intSymExpr :: IntOps Expr where
  int = eop0 "Int"
  add = eop2 "Add"
  sub = eop2 "Sub"
  mul = eop2 "Mul"
  gte = eop2 "Gte"

deserialise' :: forall r. IntOps r => ExtensibleDeserialiser r
deserialise' = dfix deserialise

deserialise :: forall r. IntOps r => ExtensibleDeserialiser r -> ExtensibleDeserialiser r
deserialise _ _ _ (Node "Int" [Leaf i])
  | Just i' <- fromString i = pure $ mkDynamic tint $ int i'
  | otherwise = Left $ "Bad int literal " <> i
deserialise _ _ _ (Node "Int" es) = Left $ "Invalid number of arguments, expected 1, found " <> show (length es)

deserialise self' _ env (Node "Add" [e1,e2]) = do
  i1 <- getInt self env e1
  i2 <- getInt self env e2
  pure $ mkDynamic tint $ add i1 i2
  where self = self' noRead
deserialise _ _ _ (Node "Add" es) = Left $ "Invalid number of arguments, expected 2, found " <> show (length es)

deserialise self' _ env (Node "Sub" [e1,e2]) = do
  i1 <- getInt self env e1
  i2 <- getInt self env e2
  pure $ mkDynamic tint $ sub i1 i2
  where self = self' noRead
deserialise _ _ _ (Node "Sub" es) = Left $ "Invalid number of arguments, expected 2, found " <> show (length es)

deserialise self' _ env (Node "Mul" [e1,e2]) = do
  i1 <- getInt self env e1
  i2 <- getInt self env e2
  pure $ mkDynamic tint $ mul i1 i2
  where self = self' noRead
deserialise _ _ _ (Node "Mul" es) = Left $ "Invalid number of arguments, expected 2, found " <> show (length es)

deserialise self' _ env (Node "Gte" [e1,e2]) = do
  i1 <- getInt self env e1
  i2 <- getInt self env e2
  pure $ mkDynamic tbool $ gte i1 i2
  where self = self' noRead
deserialise _ _ _ (Node "Gte" es) = Left $ "Invalid number of arguments, expected 2, found " <> show (length es)

deserialise self' fallback env e = fallback env e

noRead :: forall r. DynDeserialiser r
noRead _ _ = Left "Malformed type"

-- Private
getInt
  :: forall exp env r
   . (env -> exp -> Either String (Dynamic r))
  -> env -> exp -> Either String (r Int)
getInt deser env e = do
  i@(Dynamic p) <- deser env e
  p # runExists \(Dynamic' t d) ->
    maybeToEither ("invalid type of argument, expected bool, found " <> show t) $ asInt i
  where
    maybeToEither :: forall e a. e -> Maybe a -> Either e a
    maybeToEither err Nothing = Left err
    maybeToEither _ (Just a) = Right a

-- Example
intOnlyDeserialiser :: forall r. IntOps r => Deserialiser r
intOnlyDeserialiser = deserialiseWith [deserialise']

sampleTerm :: forall r h. IntOps r => r h Int
sampleTerm = (int 10 #+ int 10) #* int 10
