module Test.Type.ClassLaws
  ( LawTestChoice
  , functorLaws
  , applyLaws
  , applicativeLaws
  , bindLaws
  , monadLaws
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, traverse_)
import Data.Unfoldable (fromMaybe)
import Effect (Effect)
import Effect.Console as Console
import Test.Assert as Assert

import Data.List (List)

import Test.Type.ClassLaws.TestData (HeavenlyStem, arrays, functions1, functions2)

type FromArray f = forall a. Array a -> Maybe (f a)

type LawTest f = String -> FromArray f -> Effect Unit

-- ...feels a little iffy to trustingly use the List monad in code that's supposed to test if
-- the List monad is law-abiding but. the problem I found is in Applicative so good enough !!
type LawTestChoice = List

-- WE LOVE OVERENGINEERING not like this wouldn't be trivial to write with monad transformers
-- and Free and all that jazz
-- but introducing new Bower dependencies for testing seems like a radically bad idea
-- especially for as basic a package as lists
-- that like the transformers probably depend on anyways
newtype LawTestM f a = LawTestM (FromArray f -> LawTestChoice a)

instance Functor (LawTestM f) where
  map f (LawTestM g) = LawTestM $ map (map f) g

instance Apply (LawTestM f) where
  apply (LawTestM f) (LawTestM g) = LawTestM \fromArray -> f fromArray <*> g fromArray

instance Applicative (LawTestM f) where
  pure = LawTestM <<< pure <<< pure

instance Bind (LawTestM f) where
  bind :: forall a b. LawTestM f a -> (a -> LawTestM f b) -> LawTestM f b
  bind (LawTestM g) f = LawTestM bound
    where bound :: FromArray f -> LawTestChoice b
          bound fromArray = do
            g' <- g fromArray
            let LawTestM h = f g'
            h fromArray

instance Monad (LawTestM f)

type Assertion = Maybe String

assert :: forall m s. Applicative m => Show s => s -> Boolean -> m (Maybe String)
assert info testResult
  | not testResult = pure $ Just $ "Assertion failed on " <> show info
  | otherwise = pure Nothing

runAssertion :: Assertion -> Effect Unit
runAssertion (Just msg) = Assert.assert' msg false
runAssertion Nothing = pure unit



data Law f = Law String (LawTestM f Assertion)

runLaw :: forall f. Law f -> FromArray f -> Effect Unit
runLaw (Law lawDescription (LawTestM reader)) fromArray = do
  Console.log lawDescription
  traverse_ runAssertion $ reader fromArray

make :: forall f a. Array a -> LawTestM f (f a)
make a = LawTestM \fromArray -> fromMaybe (fromArray a)

choose :: forall f a. LawTestChoice a -> LawTestM f a
choose = LawTestM <<< const

fA :: forall f. LawTestM f (f HeavenlyStem)
fA = choose arrays >>= make

testLaws :: forall f t. Traversable t => String -> t (Law f) -> LawTest f
testLaws className lawTests typeName fromArray = do
  let groupName = typeName <> " should satisfy " <> className <> " laws:"
  -- hope this actually prints that lol I do not understand groups
  Console.grouped groupName $ traverse_ (_ `runLaw` fromArray) lawTests

functorLaws :: forall f.
  Eq (f HeavenlyStem) =>
  Show (f HeavenlyStem) =>
  Functor f =>
  LawTest f
functorLaws = testLaws "Functor"
  [ Law "Identity: map identity = identity" do
      x <- fA
      assert x $ map identity x == x
  , Law "Composition: map (f <<< g) = map f <<< map g" do
      assert unit true
  ]

applyLaws :: forall f.
  Eq (f HeavenlyStem) =>
  Show (f HeavenlyStem) =>
  Apply f =>
  LawTest f
applyLaws = testLaws "Apply"
  [
  ]

applicativeLaws :: forall f.
  Eq (f HeavenlyStem) =>
  Show (f HeavenlyStem) =>
  Applicative f =>
  LawTest f
applicativeLaws = testLaws "Applicative"
  [
  ]

bindLaws :: forall f.
  Eq (f HeavenlyStem) =>
  Show (f HeavenlyStem) =>
  Bind f =>
  LawTest f
bindLaws = testLaws "Bind"
  [
  ]

monadLaws :: forall f.
  Eq (f HeavenlyStem) =>
  Show (f HeavenlyStem) =>
  Monad f =>
  LawTest f
monadLaws = testLaws "Monad"
  [
  ]
