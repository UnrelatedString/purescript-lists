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
import Effect.Console (log)
import Test.Assert (assert)

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

type Constructor f = Array HeavenlyStem -> Maybe (f HeavenlyStem)

functorLaws :: forall f.
  Eq (f HeavenlyStem) =>
  Functor f =>
  Constructor f -> Effect Unit
functorLaws c = do
  pure unit

applyLaws :: forall f.
  Eq (f HeavenlyStem) =>
  Apply f =>
  Constructor f -> Effect Unit
applyLaws c = do
  functorLaws c

applicativeLaws :: forall f.
  Eq (f HeavenlyStem) =>
  Applicative f =>
  Constructor f -> Effect Unit
applicativeLaws c = do
  applyLaws c

bindLaws :: forall f.
  Eq (f HeavenlyStem) =>
  Bind f =>
  Constructor f -> Effect Unit
bindLaws c = do
  pure unit

monadLaws :: forall f.
  Eq (f HeavenlyStem) =>
  Monad f =>
  Constructor f -> Effect Unit
monadLaws c = do
  applicativeLaws c
  bindLaws c
