module Test.Type.ClassLaws
  ( functorLaws
  , applyLaws
  , applicativeLaws
  , bindLaws
  , monadLaws
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console as Console
import Test.Assert as Assert

type FromArray f = forall a. Array a -> Maybe (f a)

type LawsTest :: forall k. (k -> Constraint) -> Type
type LawsTest c = forall f.
  c f => 
