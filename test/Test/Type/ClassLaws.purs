module Test.Type.ClassLaws
  ( HeavenlyStem
  , functorLaws
  , applyLaws
  , applicativeLaws
  , bindLaws
  , monadLaws
  ) where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Console as Console
import Test.Assert as Assert

import Data.List (List)

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
-- but introducing new Bower dependencies for testing seems like a radically bad idea
-- especially for as basic a package as lists
newtype LawTestM f a = LawTestM (FromArray f -> LawTestM' f a)

data LawTestM' f a = 

make :: forall f a. Array a -> LawTestM f (f a)
make = LawTestM <<< pure <<< (#)

testLaws :: forall f. String -> LawTestM f Unit -> LawTest f

functorLaws :: forall f.
  Eq (f HeavenlyStem) =>
  Functor f =>
  LawTest f
functorLaws = testLaws "Functor" do
  pure unit

applyLaws :: forall f.
  Eq (f HeavenlyStem) =>
  Apply f =>
  LawTest f
applyLaws = testLaws "Apply" do
  pure unit

applicativeLaws :: forall f.
  Eq (f HeavenlyStem) =>
  Applicative f =>
  LawTest f
applicativeLaws = testLaws "Applicative" do
  pure unit

bindLaws :: forall f.
  Eq (f HeavenlyStem) =>
  Bind f =>
  LawTest f
bindLaws = testLaws "Bind" do
  pure unit

monadLaws :: forall f.
  Eq (f HeavenlyStem) =>
  Monad f =>
  LawTest f
monadLaws = testLaws "Monad" do
  pure unit
