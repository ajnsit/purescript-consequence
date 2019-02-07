{- Serialisation formats -}
module Expr where

import Data.Eq (class Eq)
import Data.Function (($))
import Data.Generic.Rep (class Generic)
import Data.Monoid ((<>))
import Data.Show (class Show, show)

---- UNTYPED -----------------------------------------

-- Extensible Untyped Serialisation format
data ExprU
  = Leaf String
  | Node String (Array ExprU)

derive instance eqExprU :: Eq ExprU
derive instance genericExprU :: Generic ExprU _

instance showExprU :: Show ExprU where
  show (Leaf s) = s
  show (Node s arr) = s <> show arr

-- TODO: Read


---- TYPED -------------------------------------------

-- Like ExprU, but carries a phantom type param `a`
--  and a phantom environment param `h`
newtype Expr h a = Expr ExprU
serialise :: forall h a. Expr h a -> ExprU
serialise (Expr e) = e

-- UTILITY
-- Basic ops for Expr
eopString :: forall h a. String -> String -> Expr h a
eopString tag s = Expr $ Node tag [Leaf s]

eop0 :: forall h a. Show a => String -> a -> Expr h a
eop0 tag a = eopString tag $ show a

-- Can't use map with serialise because then we lose newtype efficiency
-- So we have different eops for different arities

eop1 :: forall h a b. String -> Expr h a -> Expr h b
eop1 tag a = Expr $ Node tag [serialise a]

eop2 :: forall h a b c. String -> Expr h a -> Expr h b -> Expr h c
eop2 tag a b = Expr $ Node tag [serialise a, serialise b]

eop3 :: forall h a b c d. String -> Expr h a -> Expr h b -> Expr h c -> Expr h d
eop3 tag a b c = Expr $ Node tag [serialise a, serialise b, serialise c]

eopList :: forall h b. String -> Array ExprU -> Expr h b
eopList tag ls = Expr $ Node tag ls
