module Test.Type.ClassLaws
  ( HeavenlyStem
  , functorLaws
  , applyLaws
  , applicativeLaws
  , bindLaws
  , monadLaws
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, traverse_)
import Effect (Effect)
import Effect.Console as Console
import Test.Assert as Assert

import Data.List (List)
import Data.Newtype (class Newtype, modify)

-- Small-but-not-too-small type to make it easier to hard-code random inputs
-- to make up for no QuickCheck dependency

data HeavenlyStem = Jia | Yi | Bing | Ding | Wu | Ji | Geng | Xin | Ren | Gui

derive instance Eq HeavenlyStem
derive instance Ord HeavenlyStem

exhaustive :: forall a. a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> HeavenlyStem -> a
exhaustive x _ _ _ _ _ _ _ _ _ Jia = x
exhaustive _ x _ _ _ _ _ _ _ _ Yi = x
exhaustive _ _ x _ _ _ _ _ _ _ Bing = x
exhaustive _ _ _ x _ _ _ _ _ _ Ding = x
exhaustive _ _ _ _ x _ _ _ _ _ Wu = x
exhaustive _ _ _ _ _ x _ _ _ _ Ji = x
exhaustive _ _ _ _ _ _ x _ _ _ Geng = x
exhaustive _ _ _ _ _ _ _ x _ _ Xin = x
exhaustive _ _ _ _ _ _ _ _ x _ Ren = x
exhaustive _ _ _ _ _ _ _ _ _ x Gui = x

instance Show HeavenlyStem where
  show = exhaustive "甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"

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

derive instance Newtype (LawTestM f a) _

instance Functor (LawTest f) where
  map = modify $ map <<< map

-- instance Apply (LawTest f) where


type Assertion = Maybe String

assert :: forall m s. Applicative m => Show s => s -> Boolean -> m (Maybe String)
assert info testResult
  | not testResult = pure $ Just $ "Assertion failed on " <> show info
  | otherwise = pure Nothing

runAssertion :: Assertion -> Effect Unit
runAssertion (Just msg) = Assert.assert' msg false
runAssertion Nothing = pure unit

data Law f = Law String (LawTestM f Assertion)

runLaw :: forall f a. Law f -> FromArray f -> Effect Unit
runLaw (Law lawDescription (LawTestM reader)) fromArray = do
  Console.log lawDescription
  traverse_ (traverse_ runAssertion) $ reader fromArray

make :: forall f a. Array a -> LawTestM f (f a)
make = LawTestM <<< pure <<< (#)

testLaws :: forall f t. Traversable t => String -> t (Law f) -> LawTest f
testLaws className lawTests typeName fromArray = do
  Console.log $ typeName <> " should satisfy " <> className <> " laws:"
  Console.grouped $ traverse_ (runLaw fromArray) lawTests

functorLaws :: forall f.
  Eq (f HeavenlyStem) =>
  Functor f =>
  LawTest f
functorLaws = testLaws "Functor"
  [
  ]

applyLaws :: forall f.
  Eq (f HeavenlyStem) =>
  Apply f =>
  LawTest f
applyLaws = testLaws "Apply"
  [
  ]

applicativeLaws :: forall f.
  Eq (f HeavenlyStem) =>
  Applicative f =>
  LawTest f
applicativeLaws = testLaws "Applicative"
  [
  ]

bindLaws :: forall f.
  Eq (f HeavenlyStem) =>
  Bind f =>
  LawTest f
bindLaws = testLaws "Bind"
  [
  ]

monadLaws :: forall f.
  Eq (f HeavenlyStem) =>
  Monad f =>
  LawTest f
monadLaws = testLaws "Monad"
  [
  ]
