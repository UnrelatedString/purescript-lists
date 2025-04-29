module Test.Main where

import Prelude

import Effect (Effect)
import Test.QuickCheck ((===), class Testable, class Arbitrary, class Coarbitrary)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Spec.Reporter.Console (consoleReporter)
import Type.Proxy (Proxy(..))

import Data.List.Types (List, NonEmptyList)
import Data.List.ZipList (ZipList)
import Data.List.Lazy.Types as LZ

-- Proxy a is Discard ;)
type For f =
  Eq (f Value) =>
  Show (f Value) =>
  Arbitrary (f Value) =>
  Arbitrary (f (Value -> Value)) =>
  Coarbitrary (f Value) =>
  Spec (Proxy f)

type Value = Int

proxied :: forall m f. Functor m => m Unit -> m (Proxy f)
proxied = (_ $> Proxy)

main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] do
  describe "List instances" $ void do
    functorLaws :: For List
    applyLaws :: For List
    applicativeLaws :: For List
  describe "NonEmptyList instances" $ void do
    functorLaws :: For NonEmptyList
    applyLaws :: For NonEmptyList
    applicativeLaws :: For NonEmptyList

functorLaws :: forall f. Functor f => For f
functorLaws = proxied $ describe "Functor laws" do
  it "Identity: map identity = identity" do
    quickCheck \(l :: f Value) -> map identity l === l
  it "Composition: map (f <<< g) = map f <<< map g" do
    quickCheck \(l :: f Value) (f :: Value -> Value) g -> map (f <<< g) l === map f (map g l)

applyLaws :: forall f. Apply f => For f
applyLaws = proxied $ describe "Apply law" do
  it "Associative composition: (<<<) <$> f <*> g <*> h = f <*> (g <*> h)" do
    quickCheck \(f :: f (Value -> Value)) g (h :: f Value) ->
      ((<<<) <$> f <*> g <*> h) === (f <*> (g <*> h))

applicativeLaws :: forall f. Applicative f => For f
applicativeLaws = proxied $ describe "Applicative laws" do
  it "Identity: (pure identity) <*> v = v" do
    quickCheck \(v :: f Value) -> (pure identity) <*> v === v
  it "Composition: pure (<<<) <*> f <*> g <*> h = f <*> (g <*> h)" do
    quickCheck \(f :: f (Value -> Value)) g (h :: f Value) ->
      (pure (<<<) <*> f <*> g <*> h) === (f <*> (g <*> h))
  it "Homomorphism: (pure f) <*> (pure x) = pure (f x)" do
    quickCheck \f (x :: Value) -> (pure f <*> pure x) === (pure (f x) :: f Value)
  it "Interchange: u <*> (pure y) = (pure (_ $ y)) <*> u" do
    quickCheck \(u :: f (Value -> Value)) y ->
      (u <*> (pure y)) === ((pure (_ $ y)) <*> u)
