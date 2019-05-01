module Pretty where

import Control.Category ((<<<))
import Data.Function (const, ($))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)

newtype Pretty h a = Pretty (Int -> String)

derive instance prettyNewtype :: Newtype (Pretty h a) _

-- UTILITY
-- Basic ops for Pretty
sopString :: forall h a. String -> Pretty h a
sopString = wrap <<< const

sop0 :: forall h a. Show a => a -> Pretty h a
sop0 = sopString <<< show

sop1 :: forall h a b. String -> Pretty h a -> Pretty h b
sop1 op a = wrap $ \i -> "(" <> op <> " " <> unwrap a i <> ")"

sop2 :: forall h a b c. String -> Pretty h a -> Pretty h b -> Pretty h c
sop2 op a b = wrap $ \i -> "(" <> unwrap a i <> " " <> op <> " " <> unwrap b i <> ")"

sop3 :: forall h a b c d. String -> String -> String -> Pretty h a -> Pretty h b -> Pretty h c -> Pretty h d
sop3 o1 o2 o3 a b c = wrap $ \i -> "(" <> o1 <> " " <> unwrap a i <> " " <> o2 <> " " <> unwrap b i <> " " <> o3 <> " " <> unwrap c i <> ")"

-- Prettyprint something
pretty :: forall h a. Pretty h a -> String
pretty e = unwrap e 0
