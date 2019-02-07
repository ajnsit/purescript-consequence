{- Strong Type Checking for DSLs -}
module Typing where

import Expr

import Control.Applicative (pure)
import Control.Category ((>>>))
import Control.Monad (class Monad, bind)
import Data.Either (Either(..))
import Data.Eq (class Eq, (==))
import Data.Exists (Exists, Exists2, mkExists, mkExists2, runExists, runExists2)
import Data.Function (identity, (#), ($))
import Data.Functor (class Functor, map, (<$>))
import Data.Identity (Identity(..))
import Data.Leibniz (type (~), applyLeibniz, liftLeibniz, liftLeibniz1of2, runLeibniz, symm)
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Newtype (unwrap)
import Data.Show (class Show, show)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple3, get1, get2, get3, tuple3)
import Data.Unit (Unit)

-- Supported data types
class TSym p where
  -- Integers
  ttint :: p Int
  -- Booleans
  ttbool :: p Boolean
  -- Characters
  ttchar :: p Char
  -- Unit
  ttunit :: p Unit
  -- Functions
  ttarr :: forall a b. p a -> p b -> p (a -> b)
  -- Tuples (Pairs)
  tttuple :: forall a b. p a -> p b -> p (Tuple a b)
  -- Arrays
  ttarray :: forall a. p a -> p (Array a)

-- We can get values for any supported type using "typrep"
class SupportedType a where
  typrep :: forall p. TSym p => p a

instance supportedTypeInt :: SupportedType Int where
  typrep = ttint
instance supportedTypeBool :: SupportedType Boolean where
  typrep = ttbool
instance supportedTypeChar :: SupportedType Char where
  typrep = ttchar
instance supportedTypeUnit :: SupportedType Unit where
  typrep = ttunit
instance supportedTypeFunc :: (SupportedType a, SupportedType b) => SupportedType (a->b) where
  typrep = ttarr typrep typrep
instance supportedTypeTuple :: (SupportedType a, SupportedType b) => SupportedType (Tuple a b) where
  typrep = tttuple typrep typrep
instance supportedTypeArray :: SupportedType a => SupportedType (Array a) where
  typrep = ttarray typrep

-- Abstract TSym
newtype TypQ a = TypQ (forall p. TSym p => p a)

instance tSymTypQ :: TSym TypQ where
  ttunit = TypQ ttunit
  ttint = TypQ ttint
  ttbool = TypQ ttbool
  ttchar = TypQ ttchar
  ttarr (TypQ a) (TypQ b) = TypQ (ttarr a b)
  tttuple (TypQ a) (TypQ b) = TypQ (tttuple a b)
  ttarray (TypQ a) = TypQ (ttarray a)

-- Specialised TypQ construction methods
tunit :: TypQ Unit
tunit = ttunit
tint :: TypQ Int
tint = ttint
tbool :: TypQ Boolean
tbool = ttbool
tchar :: TypQ Char
tchar = ttchar
tarr :: forall a b. TypQ a -> TypQ b -> TypQ (a->b)
tarr = ttarr
infixr 4 ttarr as ~~>
ttuple :: forall a b. TypQ a -> TypQ b -> TypQ (Tuple a b)
ttuple = tttuple
tarray :: forall a. TypQ a -> TypQ (Array a)
tarray = ttarray

-- Existentially quantified
data Typ = Typ (Exists TypQ)

-- A generalised Data.Dynamic
data Dynamic t = Dynamic (forall a. Tuple (TypQ a) (t a))

-- Constructors, also carry a type equality witness
-- We don't need GADTs
newtype AsInt a = AsInt (Maybe (a ~ Int))
newtype AsBool a = AsBool (Maybe (a ~ Boolean))
newtype AsChar a = AsChar (Maybe (a ~ Char))
newtype AsUnit a = AsUnit (Maybe (a ~ Unit))
-- No existentials in Purescript :(
data AsArrow' a b1 b2 = AsArrow' (TypQ a) (Maybe (Tuple3 (TypQ b1) (TypQ b2) (a ~ (b1->b2))))
newtype AsArrow a = AsArrow (Exists2 (AsArrow' a))
data AsTuple' a b1 b2 = AsTuple' (TypQ a) (Maybe (Tuple3 (TypQ b1) (TypQ b2) (a ~ (Tuple b1 b2))))
newtype AsTuple a = AsTuple (Exists2 (AsTuple' a))
data AsArray' a b = AsArray' (TypQ a) (Maybe (Tuple (TypQ b) (a ~ Array b)))
newtype AsArray a = AsArray (Exists (AsArray' a))

-- Convert a dynamic to a Unit if possible
asUnit :: forall t. Dynamic t -> Maybe (t Unit)
asUnit (Dynamic p) = case p of
  Tuple (TypQ pp) c -> case pp of
    AsUnit Nothing -> Nothing
    AsUnit (Just tproof) -> Just (runLeibniz tproof c)

-- Convert a dynamic to an Int if possible
asInt :: forall t. Dynamic t -> Maybe (t Int)
asInt (Dynamic p) = case p of
  Tuple (TypQ pp) c -> case pp of
    AsInt Nothing -> Nothing
    AsInt (Just tproof) -> Just (runLeibniz tproof c)

-- Convert a dynamic to a Bool if possible
asBool :: forall t. Dynamic t -> Maybe (t Boolean)
asBool (Dynamic p) = case p of
  Tuple (TypQ pp) c -> case pp of
    AsBool Nothing -> Nothing
    AsBool (Just tproof) -> Just (runLeibniz tproof c)

-- Convert a dynamic to a Char if possible
asChar :: forall t. Dynamic t -> Maybe (t Char)
asChar (Dynamic p) = case p of
  Tuple (TypQ pp) c -> case pp of
    AsChar Nothing -> Nothing
    AsChar (Just tproof) -> Just (runLeibniz tproof c)

instance tSymAsUnit :: TSym AsUnit where
  ttunit = AsUnit $ Just identity
  ttint = AsUnit Nothing
  ttbool = AsUnit Nothing
  ttchar = AsUnit Nothing
  ttarr _ _ = AsUnit Nothing
  tttuple _ _ = AsUnit Nothing
  ttarray _ = AsUnit Nothing

instance tSymAsInt :: TSym AsInt where
  ttunit = AsInt Nothing
  ttint = AsInt $ Just identity
  ttbool = AsInt Nothing
  ttchar = AsInt Nothing
  ttarr _ _ = AsInt Nothing
  tttuple _ _ = AsInt Nothing
  ttarray _ = AsInt Nothing

instance tSymAsBool :: TSym AsBool where
  ttunit = AsBool Nothing
  ttint = AsBool Nothing
  ttbool = AsBool $ Just identity
  ttchar = AsBool Nothing
  ttarr _ _ = AsBool Nothing
  tttuple _ _ = AsBool Nothing
  ttarray _ = AsBool Nothing

instance tSymAsChar :: TSym AsChar where
  ttunit = AsChar Nothing
  ttint = AsChar Nothing
  ttbool = AsChar Nothing
  ttchar = AsChar $ Just identity
  ttarr _ _ = AsChar Nothing
  tttuple _ _ = AsChar Nothing
  ttarray _ = AsChar Nothing

instance tSymAsArrow :: TSym AsArrow where
  ttunit = AsArrow $ mkExists2 $ AsArrow' ttunit Nothing
  ttint = AsArrow $ mkExists2 $ AsArrow' ttint Nothing
  ttbool = AsArrow $ mkExists2 $ AsArrow' ttbool Nothing
  ttchar = AsArrow $ mkExists2 $ AsArrow' ttchar Nothing
  ttarr r1 r2 = case r1 of
    AsArrow p1 -> case r2 of
      AsArrow p2 -> p1 # runExists2 \(AsArrow' t1 _) ->
        p2 # runExists2 \(AsArrow' t2 _) ->
          AsArrow $ mkExists2 $ AsArrow' (ttarr t1 t2) $ Just (tuple3 t1 t2 identity)
  tttuple r1 r2 = case r1 of
    AsArrow p1 -> case r2 of
      AsArrow p2 -> p1 # runExists2 \(AsArrow' t1 _) ->
        p2 # runExists2 \(AsArrow' t2 _) ->
          AsArrow $ mkExists2 $ AsArrow' (tttuple t1 t2) Nothing
  ttarray r = case r of
    AsArrow p -> p # runExists2 \(AsArrow' t _) ->
      AsArrow $ mkExists2 $ AsArrow' (ttarray t) Nothing

instance tSymAsTuple :: TSym AsTuple where
  ttunit = AsTuple $ mkExists2 $ AsTuple' ttunit Nothing
  ttint = AsTuple $ mkExists2 $ AsTuple' ttint Nothing
  ttbool = AsTuple $ mkExists2 $ AsTuple' ttbool Nothing
  ttchar = AsTuple $ mkExists2 $ AsTuple' ttchar Nothing
  ttarr r1 r2 = case r1 of
    AsTuple p1 -> case r2 of
      AsTuple p2 -> p1 # runExists2 \(AsTuple' t1 _) ->
        p2 # runExists2 \(AsTuple' t2 _) ->
          AsTuple $ mkExists2 $ AsTuple' (ttarr t1 t2) Nothing
  tttuple r1 r2 = case r1 of
    AsTuple p1 -> case r2 of
      AsTuple p2 -> p1 # runExists2 \(AsTuple' t1 _) ->
        p2 # runExists2 \(AsTuple' t2 _) ->
          AsTuple $ mkExists2 $ AsTuple' (tttuple t1 t2) $ Just (tuple3 t1 t2 identity)
  ttarray r = case r of
    AsTuple p -> p # runExists2 \(AsTuple' t _) ->
      AsTuple $ mkExists2 $ AsTuple' (ttarray t) Nothing

instance tSymAsArray :: TSym AsArray where
  ttunit = AsArray $ mkExists $ AsArray' ttunit Nothing
  ttint = AsArray $ mkExists $ AsArray' ttint Nothing
  ttbool = AsArray $ mkExists $ AsArray' ttbool Nothing
  ttchar = AsArray $ mkExists $ AsArray' ttchar Nothing
  ttarr r1 r2 = case r1 of
    AsArray p1 -> case r2 of
      AsArray p2 -> p1 # runExists \(AsArray' t1 _) ->
        p2 # runExists \(AsArray' t2 _) ->
          AsArray $ mkExists $ AsArray' (ttarr t1 t2) Nothing
  tttuple r1 r2 = case r1 of
    AsArray p1 -> case r2 of
      AsArray p2 -> p1 # runExists \(AsArray' t1 _) ->
        p2 # runExists \(AsArray' t2 _) ->
          AsArray $ mkExists $ AsArray' (tttuple t1 t2) Nothing
  ttarray r = case r of
    AsArray p -> p # runExists \(AsArray' t _) ->
      AsArray $ mkExists $ AsArray' (ttarray t) $ Just (Tuple t identity)

-- Type checking using TypQs
-- SafeCast a allows you to potentially extract a witness of type equality with a
newtype SafeCast a = SafeCast (forall b. TypQ b -> Maybe (a ~ b))
instance tsymSafeCast :: TSym SafeCast where
  ttunit = SafeCast (\(TypQ p) -> case p of AsUnit castf -> _runCast0 castf)
  ttint = SafeCast (\(TypQ p) -> case p of AsInt castf -> _runCast0 castf)
  ttbool = SafeCast (\(TypQ p) -> case p of AsBool castf -> _runCast0 castf)
  ttchar = SafeCast (\(TypQ p) -> case p of AsChar castf -> _runCast0 castf)
  ttarray (SafeCast t) =
    SafeCast (\(TypQ r) -> case r of AsArray p -> p # runExists \(AsArray' _ castf) -> _runCast1 castf t)
  ttarr (SafeCast t1) (SafeCast t2) =
    SafeCast (\(TypQ r) -> case r of AsArrow p -> p # runExists2 \(AsArrow' _ castf) -> _runCast2 castf t1 t2)
  tttuple (SafeCast t1) (SafeCast t2) =
    SafeCast (\(TypQ r) -> case r of AsTuple p -> p # runExists2 \(AsTuple' _ castf) -> _runCast2 castf t1 t2)

_runCast0
  :: forall m a b
   . Functor m
  => m (a ~ b)
  -> m (b ~ a)
_runCast0 castf = map symm castf

_runCast1
  :: forall m t t1 a b c
   . Monad m
  => m (Tuple t (c ~ t1 b))
  -> (t -> m (a ~ b))
  -> m (t1 a ~ c)
_runCast1 castf t = do
  Tuple b eqBb <- castf
  eqTb <- t b
  pure $ eqTrans1 eqTb >>> symm eqBb

_runCast2
  :: forall m t1 t2 t3 a b c d e
   . Monad m
  => m (Tuple3 t1 t2 (e ~ t3 b d))
  -> (t1 -> m (a ~ b))
  -> (t2 -> m (c ~ d))
  -> m (t3 a c ~ e)
_runCast2 castf t1 t2 = do
  res <- castf
  let b1 = get1 res
  let b2 = get2 res
  let eqBb1b2 = get3 res
  eqT1b1 <- t1 b1
  eqT2b2 <- t2 b2
  pure $ eqTrans2 eqT1b1 eqT2b2 >>> symm eqBb1b2


-------------
-- UTILITY --
-------------

-- Casting equal constructs
eqCast :: forall t a b. a ~ b -> t a -> t b
eqCast = runLeibniz

-- TODO: Temporarily commented out
-- eqCast2 :: forall a b c d t. (a ~ c) -> (b ~ d) -> t a b -> t c d
-- eqCast2 tproof1 tproof2 = identity
-- eqCast3 :: forall a b c d e f t. (a ~ d) -> (b ~ e) -> (c ~ f) -> t a b c -> t d e f
-- eqCast3 tproof1 tproof2 tproof3 = identity

-- Prove equality over arbitrary "transformers"
eqTrans1 :: forall a b. (a ~ b) -> forall t. t a ~ t b
eqTrans1 = liftLeibniz
eqTrans2 :: forall a b c d. (a ~ b) -> (c ~ d) -> forall t. t a c ~ t b d
eqTrans2 tproofab tproofcd = applyLeibniz (liftLeibniz1of2 tproofab) tproofcd

------------------------------------
-- Data.Typeable Interface
------------------------------------

-- AJ
-- Extract a witness of type equality
eqT :: forall a b. TypQ a -> TypQ b -> Maybe (a ~ b)
eqT (TypQ p) = case p of SafeCast f -> f

-- Safe cast
cast :: forall a b. TypQ a -> TypQ b -> a -> Maybe b
cast ta tb a = gen a <$> eqT ta tb
  where
    gen :: a -> a ~ b -> b
    gen c tproof = unwrap (runLeibniz tproof (Identity c))

-- Safe construct cast
gcast :: forall a b c. TypQ a -> TypQ b -> c a -> Maybe (c b)
gcast ta tb ca = gen ca <$> eqT ta tb
 where
   gen :: c a -> a ~ b -> c b
   gen c tproof = runLeibniz tproof c

-------------------------------------------------------------------------
-- SERIALISATION

-- HACK: To compare Typ, compare serial representations
instance eqTyp :: Eq Typ where
  eq (Typ t1) (Typ t2) = t1 # runExists \(TypQ p1) -> t2 # runExists \(TypQ p2) ->
    let ShowT s1 = p1
        ShowT s2 = p2
    in s1 == s2

-- Show instance for TypQ
newtype ShowT a = ShowT String
instance tSymShowT :: TSym ShowT where
  ttunit = ShowT "Unit"
  ttbool = ShowT "Bool"
  ttchar = ShowT "Char"
  ttint = ShowT "Int"
  ttarr (ShowT a) (ShowT b) = ShowT $ "(" <> a <> " -> " <> b <> ")"
  tttuple (ShowT a) (ShowT b) = ShowT $ "(" <> a <> ", " <> b <> ")"
  ttarray (ShowT a) = ShowT $ "[" <> a <> "]"

instance showTypQ :: Show (TypQ a) where
  show (TypQ p) = let ShowT s = p in s

typToExp :: Typ -> ExprU
typToExp (Typ t) = t # runExists _tqToExp

_tqToExp :: forall a. TypQ a -> ExprU
_tqToExp (TypQ t) = serialise t

expToTyp :: ExprU -> Either String Typ
-- Note: We use Nodes instead of Leafs for int/bool/str so that -
-- the representation for types is similar to the representation for values
expToTyp (Node "TUnit" [])    = pure $ Typ $ mkExists ttunit
expToTyp (Node "TInt" [])     = pure $ Typ $ mkExists ttint
expToTyp (Node "TBool" [])    = pure $ Typ $ mkExists ttbool
expToTyp (Node "TChar" [])  = pure $ Typ $ mkExists ttchar
expToTyp (Node "TArr" [e1,e2]) = do
  Typ t1 <- expToTyp e1
  Typ t2 <- expToTyp e2
  pure $ t1 # runExists \f1 -> t2 # runExists \f2 -> Typ (mkExists (ttarr f1 f2))
expToTyp (Node "TTuple" [e1,e2]) = do
  Typ t1 <- expToTyp e1
  Typ t2 <- expToTyp e2
  pure $ t1 # runExists \f1 -> t2 # runExists \f2 -> Typ (mkExists (ttuple f1 f2))
expToTyp (Node "TArray" [e]) = do
  Typ t <- expToTyp e
  pure $ t # runExists \f -> Typ (mkExists (ttarray f))
expToTyp e = Left $ "Bad type expression: " <> show e

instance tSymExpr :: TSym (Expr h) where
  ttunit = Expr $ Node "TUnit" []
  ttint = Expr $ Node "TInt" []
  ttbool = Expr $ Node "TBool" []
  ttchar = Expr $ Node "TChar" []
  ttarr a b = Expr $ Node "TArr" [serialise a, serialise b]
  tttuple a b = Expr $ Node "TTuple" [serialise a, serialise b]
  ttarray a = Expr $ Node "TArray" [serialise a]
