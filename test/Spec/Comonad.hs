{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Test.Tasty.QuickCheck.Laws.Comonad
-- Description : Tasty trees of QuickCheck properties for Comonad laws
-- Copyright   : (c) Boris Buliga, 2020
-- License     : MIT
-- Maintainer  : boris@d12frosted.io
-- Stability   : experimental
-- Portability : POSIX
--
-- Ready to use tasty trees of QuickCheck properties for @Comonad@ laws. To get
-- started, take a look at @testComonadLaws@.
module Spec.Comonad
  ( testComonadLaws,
    testComonadLawRightIdentity,
    testComonadLawLeftIdentity,
    testComonadLawAssociativity,
  )
where

--------------------------------------------------------------------------------

import Control.Comonad
import Data.Data (Proxy)
import Data.Typeable (Typeable, typeRep)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( Arbitrary (..),
    CoArbitrary (..),
    testProperty,
  )
import Text.Show.Functions ()

--------------------------------------------------------------------------------

-- | Constructs a @TestTree@ checking that the @Comonad@ class laws hold for @w@
-- with value types @a@, @b@, and @c@, using a given equality test for values of
-- type @forall u. w u@ and @forall u. u@. The equality context type @t@ is for
-- constructors @m@ from which we can only extract a value within a context,
-- such as reader-like constructors.
testComonadLaws ::
  ( Comonad w,
    Eq a,
    Eq b,
    Eq c,
    Show t,
    Show (w a),
    Show (w b),
    Show (w c),
    Arbitrary t,
    Arbitrary (w a),
    Arbitrary b,
    Arbitrary c,
    CoArbitrary (w a),
    CoArbitrary (w b),
    Typeable w,
    Typeable a,
    Typeable b,
    Typeable c
  ) =>
  -- | Type constructor under test
  Proxy w ->
  -- | Equality context for @w@
  Proxy t ->
  -- | Value type
  Proxy a ->
  -- | Value type
  Proxy b ->
  -- | Value type
  Proxy c ->
  -- | Equality test
  (forall u. (Eq u) => t -> w u -> w u -> Bool) ->
  -- | Equality test
  (forall u. (Eq u) => t -> u -> u -> Bool) ->
  TestTree
testComonadLaws pw pt pa pb pc eq eq0 =
  let label =
        "Comonad L22aws for " ++ show (typeRep pw) ++ " with "
          ++ "a :: "
          ++ show (typeRep pa)
          ++ ", "
          ++ "b :: "
          ++ show (typeRep pb)
          ++ ", "
          ++ "c :: "
          ++ show (typeRep pc)
   in testGroup
        label
        [ testComonadLawRightIdentity pw pt pa eq,
          testComonadLawLeftIdentity pw pt pa pb eq0,
          testComonadLawAssociativity pw pt pa pb pc eq
        ]

--------------------------------------------------------------------------------

-- | @extend extract === id@
testComonadLawRightIdentity ::
  ( Comonad w,
    Eq a,
    Show t,
    Show (w a),
    Arbitrary t,
    Arbitrary (w a)
  ) =>
  -- | Type constructor under test
  Proxy w ->
  -- | Equality context for @w@
  Proxy t ->
  -- | Value type
  Proxy a ->
  -- | Equality test
  (forall u. (Eq u) => t -> w u -> w u -> Bool) ->
  TestTree
testComonadLawRightIdentity pw pt pa eq =
  testProperty "extend extract === id" $ comonadLawRightIdentity pw pt pa eq

comonadLawRightIdentity ::
  (Comonad w, Eq a) =>
  -- | Type constructor under test
  Proxy w ->
  -- | Equality context for @w@
  Proxy t ->
  -- | Value type
  Proxy a ->
  -- | Equality test
  (forall u. (Eq u) => t -> w u -> w u -> Bool) ->
  t ->
  w a ->
  Bool
comonadLawRightIdentity _ _ _ eq t x = eq t (extend extract x) x

--------------------------------------------------------------------------------

-- | @extract . extend f === f@
testComonadLawLeftIdentity ::
  ( Comonad w,
    Eq b,
    Show t,
    Show (w a),
    Arbitrary t,
    Arbitrary (w a),
    Arbitrary b,
    CoArbitrary (w a)
  ) =>
  -- | Type constructor under test
  Proxy w ->
  -- | Equality context for @w@
  Proxy t ->
  -- | Value type
  Proxy a ->
  -- | Equality test
  Proxy b ->
  -- | Equality test
  (forall u. (Eq u) => t -> u -> u -> Bool) ->
  TestTree
testComonadLawLeftIdentity pw pt pa pb eq =
  testProperty "extract . extend f === f" $ comonadLawLeftIdentity pw pt pa pb eq

comonadLawLeftIdentity ::
  (Comonad w, Eq b) =>
  -- | Type constructor under test
  Proxy w ->
  -- | Equality context for @w@
  Proxy t ->
  -- | Value type
  Proxy a ->
  -- | Value type
  Proxy b ->
  -- | Equality test
  (forall u. (Eq u) => t -> u -> u -> Bool) ->
  t ->
  w a ->
  (w a -> b) ->
  Bool
comonadLawLeftIdentity _ _ _ _ eq t x f = eq t (extract . extend f $ x) (f x)

--------------------------------------------------------------------------------

-- | @extend f . extend g = extend (f . extend g)@
testComonadLawAssociativity ::
  ( Comonad w,
    Eq c,
    Show t,
    Show (w a),
    Show (w b),
    Show (w c),
    Arbitrary t,
    Arbitrary (w a),
    Arbitrary b,
    Arbitrary c,
    CoArbitrary (w a),
    CoArbitrary (w b)
  ) =>
  -- | Type constructor under test
  Proxy w ->
  -- | Equality context for @w@
  Proxy t ->
  -- | Value type
  Proxy a ->
  -- | Value type
  Proxy b ->
  -- | Value type
  Proxy c ->
  -- | Equality test
  (forall u. (Eq u) => t -> w u -> w u -> Bool) ->
  TestTree
testComonadLawAssociativity pw pt pa pb pc eq =
  testProperty "extend f . extend g = extend (f . extend g)" $
    comonadLawAssociativity pw pt pa pb pc eq

comonadLawAssociativity ::
  ( Comonad w,
    Eq c,
    Show (w a),
    Show (w b),
    Show (w c)
  ) =>
  Proxy w ->
  Proxy t ->
  Proxy a ->
  Proxy b ->
  Proxy c ->
  (forall u. (Eq u) => t -> w u -> w u -> Bool) ->
  t ->
  w a ->
  (w b -> c) ->
  (w a -> b) ->
  Bool
comonadLawAssociativity _ _ _ _ _ eq t x f g =
  eq t (extend f . extend g $ x) (extend (f . extend g) x)
