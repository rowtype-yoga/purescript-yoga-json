module Test.BasicsSpec where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.JSDate as JSDate
import Data.List as List
import Data.List.Lazy as LazyList
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe')
import Data.Newtype (class Newtype, un)
import Data.Nullable as Nullable
import Data.String.NonEmpty (NonEmptyString, nes)
import Data.Time.Duration (Days(..), Hours(..), Milliseconds(..), Minutes(..), Seconds(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant, inj)
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Foreign (ForeignError(..))
import Foreign.Object as Object
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Util (roundtrips)
import Type.Proxy (Proxy(..))
import Yoga.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)
import Yoga.JSON.Variant (TaggedVariant(..), UntaggedVariant(..))
import Yoga.Tree (Tree, mkLeaf, mkTree, showTree)

spec ∷ Spec Unit
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
      roundtrips { empty: Nothing ∷ Maybe Int }
    it "roundtrips Nullable" $ traverse_ roundtrips [ Nullable.notNull 3, Nullable.null ]
    it "roundtrips Either" do
      roundtrips ((Left 3) ∷ Either Int Int)
      roundtrips ((Right 3) ∷ Either String Int)
      writeJSON (Right 3 ∷ Either Int Int) `shouldEqual` """{"value":3,"type":"right"}"""
      writeJSON (Left true ∷ Either Boolean Int) `shouldEqual` """{"value":true,"type":"left"}"""
    it "roundtrips Tuple" $ do
      roundtrips (Tuple 3 4)
      roundtrips ("4" /\ 8 /\ Just 4)
    it "roundtrips Array" $ traverse_ roundtrips [ [ "A", "B" ], [] ]
    it "roundtrips LazyList" $ traverse_ roundtrips (LazyList.fromFoldable [ [ "A", "B" ], [] ])
    it "roundtrips List" $ traverse_ roundtrips (List.fromFoldable [ [ "A", "B" ], [] ])
    it "roundtrips NonEmptyArray" $ roundtrips (NEA.cons' "A" [ "B" ])
    it "roundtrips Object" $ roundtrips (Object.fromHomogeneous { a: 12, b: 54 })
    it "roundtrips String Map" $ roundtrips (Map.fromFoldable [ ("A" /\ 8), ("C" /\ 7) ])
    it "roundtrips Int Map" $ roundtrips (Map.fromFoldable [ (4 /\ "B"), (8 /\ "D") ])
    it "roundtrips Map with String newtype keys"
      $ roundtrips (Map.fromFoldable [ (Stringy "A" /\ "B"), (Stringy "C" /\ "D") ])
    it "roundtrips Map with Int newtype keys"
      $ roundtrips (Map.fromFoldable [ (Inty 4 /\ "B"), (Inty 8 /\ "D") ])
    it "roundtrips Map with BigInt newtype keys"
      $ roundtrips (Map.fromFoldable [ (BigInty (BigInt.fromInt 5) /\ "B"), (BigInty big /\ "D") ])
    it "roundtrips BigInt" do
      let
        inputStr = """{ "number":1, "big": 18014398509481982, "mediumBig": 1652955871799, "smallBig": 10 }"""
        expected = """{"smallBig":"10","number":1,"mediumBig":"1652955871799","big":"18014398509481982"}"""

        parsed ∷ _ ({ number ∷ Int, mediumBig ∷ BigInt, big ∷ BigInt, smallBig ∷ BigInt })
        parsed = readJSON inputStr
        stringified = parsed <#> writeJSON
      stringified `shouldEqual` (Right expected)

    it "roundtrips BigInt (2)" do
      let
        smallBig = BigInt.fromInt 10

        expected = { big, smallBig }
        json = writeJSON expected

        parsed ∷ _ ({ big ∷ BigInt, smallBig ∷ BigInt })
        parsed = readJSON json
      parsed `shouldEqual` (Right expected)

  describe "works with JSDate" do
    it "roundtrips" do
      now ← JSDate.now # liftEffect
      roundtrips now
      someDate ← JSDate.parse "2022-01-01:00:00:00Z" # liftEffect
      let result = writeJSON someDate
      let expected = show "2022-01-01T00:00:00.000Z"
      result `shouldEqual` expected

  describe "works with DateTime" do
    it "roundtrips" do
      now ← nowDateTime # liftEffect
      roundtrips now
      someDate ← JSDate.parse "2022-01-01:00:00:00Z" <#> (JSDate.toDateTime >>> fromMaybe' \_ → unsafeCrashWith "nope") # liftEffect
      let result = writeJSON someDate
      let expected = show "2022-01-01T00:00:00.000Z"
      result `shouldEqual` expected

  describe "works with Durations" do
    it "roundtrips" do
      let millis = Milliseconds 16.67
      roundtrips millis
      writeJSON millis `shouldEqual` "16.67"

      let seconds = Seconds 60.0
      roundtrips seconds
      writeJSON seconds `shouldEqual` "60"

      let minutes = Minutes 10.0
      roundtrips minutes
      writeJSON minutes `shouldEqual` "10"

      let hours = Hours 24.0
      roundtrips hours
      writeJSON hours `shouldEqual` "24"

      let days = Days 365.0
      roundtrips days
      writeJSON days `shouldEqual` "365"

  describe "works on record types" do
    it "roundtrips" do
      roundtrips { a: 12, b: "54" }
      roundtrips { a: 12, b: { c: "54" } }

  describe "works on newtypes" do
    it "roundtrips" do
      roundtrips (Stringy "A string is here")

  describe "works on variant types" do
    it "roundtrips" do
      roundtrips (inj (Proxy ∷ _ "erwin") "e" ∷ Variant ("erwin" ∷ String))
      roundtrips (inj (Proxy ∷ _ "jackie") 7 ∷ Variant ExampleVariant)

  describe "works on tagged variant types" do
    it "roundtrips" do
      roundtrips (TaggedVariant (erwin "e") ∷ TaggedVariant "super" "hans" (Erwin ()))
      let bareVariant = erwin "e"
      let res = writeJSON (TaggedVariant bareVariant ∷ TaggedVariant "type" "value" ExampleVariant)
      let expected = """{"value":"e","type":"erwin"}"""
      res `shouldEqual` expected
      let
        parsed ∷ _ (TaggedVariant "type" "value" ExampleVariant)
        parsed = readJSON expected
      (un TaggedVariant <$> parsed) `shouldEqual` Right bareVariant

  describe "works on untagged variant types" do
    it "roundtrips" do
      roundtrips (UntaggedVariant (erwin "e") ∷ UntaggedVariant (Erwin ()))
      let bareVariant = erwin "e"
      let res = writeJSON (UntaggedVariant bareVariant ∷ UntaggedVariant ExampleVariant)
      let expected = "\"e\""
      res `shouldEqual` expected
      let
        parsed ∷ _ (UntaggedVariant ExampleVariant)
        parsed = readJSON expected
      (un UntaggedVariant <$> parsed) `shouldEqual` Right bareVariant

  describe "works on non empty strings" do
    it "roundtrips NonEmptyString" do
      roundtrips (nes (Proxy ∷ Proxy "Non-Empty"))
    it "fails to decode empty strings" do
      let (result ∷ (Either _ NonEmptyString)) = readJSON (show "")
      result `shouldEqual` Left (pure $ ForeignError "String must not be empty")

  describe "works on trees" do
    it "roundtrips" do
      let t = mkTree "a" [ mkTree "b" [ mkLeaf "c", mkLeaf "d" ] ]
      roundtrips (ShowTree t)

    it "encodes as expected" do
      let t = mkTree "a" [ mkTree "b" [ mkLeaf "c", mkLeaf "d" ] ]
      writeJSON (ShowTree t) `shouldEqual`
        """{"value":"a","children":[{"value":"b","children":[{"value":"c"},{"value":"d"}]}]}"""

newtype ShowTree = ShowTree (Tree String)

instance Show ShowTree where
  show (ShowTree t) = showTree t

derive newtype instance WriteForeign ShowTree
derive newtype instance ReadForeign ShowTree
derive newtype instance Eq ShowTree

type ExampleVariant = ("erwin" ∷ String, "jackie" ∷ Int)
type ExampleTaggedVariant t v = TaggedVariant t v ExampleVariant

erwin ∷ ∀ a r. a → Variant (erwin ∷ a | r)
erwin = inj (Proxy ∷ Proxy "erwin")

type Erwin r = (erwin ∷ String | r)

newtype Stringy = Stringy String

derive instance Newtype Stringy _
derive newtype instance Show Stringy
derive newtype instance Eq Stringy
derive newtype instance Ord Stringy
derive newtype instance WriteForeign Stringy
derive newtype instance ReadForeign Stringy

newtype Inty = Inty Int

derive instance Newtype Inty _
derive newtype instance Show Inty
derive newtype instance Eq Inty
derive newtype instance Ord Inty
derive newtype instance WriteForeign Inty
derive newtype instance ReadForeign Inty

newtype BigInty = BigInty BigInt

derive instance Newtype BigInty _
derive newtype instance Show BigInty
derive newtype instance Eq BigInty
derive newtype instance Ord BigInty
derive newtype instance WriteForeign BigInty
derive newtype instance ReadForeign BigInty

big = unsafePartial $ fromJust $ BigInt.fromString "18014398509481982"
