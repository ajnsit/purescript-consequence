module Deserialiser where

import Typing

import Control.Bind (bind, pure)
import Control.Lazy (fix)
import Data.Array (uncons)
import Data.Either (Either(..))
import Data.Exists (runExists)
import Data.Function ((#), ($))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (show)
import Expr (ExprU)
import Unsafe.Coerce (unsafeCoerce)

-- The runtime environment (h) corresponding to the compile time environment (e)
data RT e

newtype UnwrapEnv r e h = UnwrapEnv (Env r e h => e -> Either String (Dynamic (r h)))

class Env r e h | e -> h where
  -- findvar :: String -> e -> Either String (Dynamic (r h))
  mkvarz :: e -> Either String (Dynamic (r h))
  mkvars :: (forall e' h'. UnwrapEnv r e' h') -> e -> Either String (Dynamic (r h))

-- These are safe, thanks to the functional dependency
fromRT :: forall r e h. Env r e h => RT e -> h
fromRT = unsafeCoerce
toRT :: forall r e h. Env r e h => h -> RT e
toRT = unsafeCoerce

-- Typed deserialisation
-- Deserialise a value of a specific type, and in a specific environment
type Deserialiser r = forall a e. TypQ a -> e -> ExprU -> Either String (r (RT e) a)

-- Try to deserialise a value of an unknown type and wrap it in a dynamic
type DynDeserialiser r = forall e. e -> ExprU -> Either String (Dynamic (r (RT e)))

-- Deserialiser versions with type args fully specified
type Deserialiser' env exp r a = env -> exp -> Either String (r (RT env) a)
type DynDeserialiser' env exp r = env -> exp -> Either String (Dynamic (r (RT env)))

-- Extensible Deserialisers
-- Takes a "fallback" deserialiser
type ExtensibleDeserialiser r = DynDeserialiser r -> DynDeserialiser r

type FixDeserialiser r = ExtensibleDeserialiser r -> ExtensibleDeserialiser r
dfix :: forall r. FixDeserialiser r -> ExtensibleDeserialiser r
dfix = fix

-- Compose Multiple ExtensibleDeserialisers together
deserialiseWith :: forall r. Array (ExtensibleDeserialiser r) -> Deserialiser r
deserialiseWith arrDeser = \tqb env e -> do
  Dynamic p <- dynDeserialiser env e
  p # runExists \(Dynamic' tqa c) -> case gcast tqa tqb c of
    Nothing -> Left ("Type mismatch: Expected type (" <> show tqb <> "), but found type (" <> show tqa <> ") in expression (" <> show e <> ")")
    Just t -> pure t
  where
    dynDeserialiser = deserialiseDynWith arrDeser

-- Like `deserialiseWith` but doesn't require type information, and returns a Dynamic
deserialiseDynWith :: forall r. Array (ExtensibleDeserialiser r) -> DynDeserialiser r
deserialiseDynWith ds = case uncons ds of
  Nothing -> \_ e -> Left $ "Malformed Expression: (" <> show e <> ")"
  Just p -> p.head (deserialiseDynWith p.tail)
