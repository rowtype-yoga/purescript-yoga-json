module Test.BasicsSpec where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Foldable (traverse_)
import Data.List as List
import Data.List.Lazy as LazyList
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Nullable as Nullable
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant, inj)
import Foreign.Object as Object
import Test.Spec (Spec, describe, it)
import Test.Util (roundtrips)
import Type.Proxy (Proxy(..))
import Yoga.JSON (class ReadForeign, class WriteForeign)

spec :: Spec Unit
spec = describe "En- and decoding" $ do

  describe "works on primitive types" do
    it "roundtrips Number" $ roundtrips 3.1414
    it "roundtrips Int" $ roundtrips (-200)
    it "roundtrips Char" $ roundtrips 'a'
    it "roundtrips String" $ roundtrips "A"

  describe "works on containers types" do
    it "roundtrips Maybe" do
      roundtrips (Just 3)
      -- just roundtrips Nothing doesn't work when rendering the JSON
      roundtrips { empty: Nothing :: Maybe Int }
    it "roundtrips Nullable" $ traverse_ roundtrips [Nullable.notNull 3, Nullable.null]
    it "roundtrips Tuple" $ do
      roundtrips (Tuple 3 4)
      roundtrips ("4" /\ 8 /\ Just 4)
    it "roundtrips Array" $ traverse_ roundtrips [["A", "B"],[]]
    it "roundtrips LazyList" $ traverse_ roundtrips (LazyList.fromFoldable [["A", "B"],[]])
    it "roundtrips List" $ traverse_ roundtrips (List.fromFoldable [["A", "B"],[]])
    it "roundtrips NonEmptyArray" $ roundtrips (NEA.cons' "A" ["B"])
    it "roundtrips Object" $ roundtrips (Object.fromHomogeneous { a: 12, b: 54 })

  describe "works on record types" do
    it "roundtrips" do
      roundtrips { a: 12, b: "54" }
      roundtrips { a: 12, b: { c: "54" } }

  describe "works on newtypes" do
    it "roundtrips" do
      roundtrips (Stringy "A string is here")

  describe "works on variant types" do
    it "roundtrips" do
      roundtrips (inj (Proxy :: Proxy "erwin") "e" :: Variant ("erwin" :: String))
      roundtrips (inj (Proxy :: Proxy "jackie") "j" :: Variant ("erwin" :: String, "jackie" :: String))

newtype Stringy = Stringy String
derive instance Newtype Stringy _
derive newtype instance Show Stringy
derive newtype instance Eq Stringy
derive newtype instance WriteForeign Stringy
derive newtype instance ReadForeign Stringy