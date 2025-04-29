module Test.ClassLaws where

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

proxied :: forall m f. m Unit -> m (Proxy f)
proxied = (_ $> Proxy)

main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] do
  describe 
  functorLaws :: For List

functorLaws :: forall f. For f
functorLaws = proxied $ describe "Functor laws" do
  it "Identity: map identity = identity" do
    quickCheck \(l :: f Value) -> map identity l === l
  it "Composition: map (f <<< g) = map f <<< map g" do
    quickCheck \(l :: f Value) -> map (f <<< g) l === map f (map g l)

