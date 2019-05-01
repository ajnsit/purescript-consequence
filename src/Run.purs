module Run where

import Control.Category ((<<<))
import Data.Function (const, ($))
import Data.Newtype (class Newtype, unwrap, wrap)

-- Runtime representation for values -
--   runtime environment (h) -> the actual value (a)
newtype Run h a = Run (h -> a)

derive instance runNewtype :: Newtype (Run h a) _

run :: forall h a. Run h a -> h -> a
run = unwrap

-- UTILITY
-- Basic ops for Run
rop0 :: forall h a. a -> Run h a
rop0 = wrap <<< const

rop1 :: forall h a b. (a -> b) -> Run h a -> Run h b
rop1 f a = wrap $ \h -> f (run a h)

rop2 :: forall h a b c. (a -> b -> c) -> Run h a -> Run h b -> Run h c
rop2 f a b = wrap $ \h -> f (run a h) (run b h)

rop3 :: forall h a b c d. (a -> b -> c -> d) -> Run h a -> Run h b -> Run h c -> Run h d
rop3 f a b c = wrap $ \h -> f (run a h) (run b h) (run c h)
