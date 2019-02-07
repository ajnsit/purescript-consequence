-- Poor man's existential types (from purescript-exists)
module Data.Exists where

import Unsafe.Coerce (unsafeCoerce)

foreign import data Exists :: (Type -> Type) -> Type
foreign import data Exists2 :: (Type -> Type -> Type) -> Type

mkExists :: forall f a. f a -> Exists f
mkExists = unsafeCoerce

runExists :: forall f r. (forall a. f a -> r) -> Exists f -> r
runExists = unsafeCoerce

mkExists2 :: forall f a b. f a b -> Exists2 f
mkExists2 = unsafeCoerce

runExists2 :: forall f r. (forall a b. f a b -> r) -> Exists2 f -> r
runExists2 = unsafeCoerce
