module Test.GenericsSpec where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.Extra (snakeCase)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Util (roundtrips)
import Yoga.JSON (class ReadForeign, class WriteForeign, writeJSON)
import Yoga.JSON.Generics (readGenericTaggedSum, writeGenericTaggedSum)
import Yoga.JSON.Generics as GenericTaggedSum
import Yoga.JSON.Generics.EnumSumRep (readGenericEnum, writeGenericEnum)
import Yoga.JSON.Generics.TaggedSumRep (defaultOptions)
import Yoga.JSON.Generics.UntaggedProductRep (readGenericUntaggedProduct, writeGenericUntaggedProduct)
import Yoga.JSON.Generics.UntaggedSumRep (readGenericUntaggedSum, writeGenericUntaggedSum)

spec ∷ Spec Unit
spec = describe "Generics" $ do

  describe "Untagged" do

    describe "IntOrString = AnInt Int | AString String" do
      it "roundtrips" do
        roundtrips (AnInt 1)
        roundtrips (AString "Abc")
      it "serialises without tags" do
        writeJSON (AnInt 1) `shouldEqual` "1"
        writeJSON (AString "Abc") `shouldEqual` "\"Abc\""

    describe "data DoubleTrouble = IntAndString Int String" do
      it "roundtrips" do
        roundtrips (IntAndString 1 "Freddy")
      it "serialises without tags" do
        writeJSON (IntAndString 1 "Freddy") `shouldEqual` "[1,\"Freddy\"]"

  describe "Tagged" do

    describe "IntOrStringTagged = ATaggedInt Int | ATaggedString String" do
      it "roundtrips" do
        roundtrips (ATaggedInt 1)
        roundtrips (ATaggedString "HOHOHOHO")

      it "serialises with tags" do
        writeJSON (ATaggedInt 1) `shouldEqual` """{"type":"ATaggedInt","value":1}"""
        writeJSON (ATaggedString "Abc") `shouldEqual` """{"type":"ATaggedString","value":"Abc"}"""

    describe "data HalfEnum = NotEnum Int | IsEnum" do
      it "roundtrips" do
        roundtrips (NotEnum 1)
        roundtrips (IsEnum)

      it "serialises with tags" do
        writeJSON (NotEnum 1) `shouldEqual` """{"kind":"not_enum","data":1}"""
        writeJSON (IsEnum) `shouldEqual` """{"kind":"is_enum"}"""

data HalfEnum = NotEnum Int | IsEnum
derive instance Generic HalfEnum _
derive instance Eq HalfEnum
instance Show HalfEnum where show = genericShow
halfEnumOptions :: GenericTaggedSum.Options
halfEnumOptions = { typeTag: "kind", valueTag: "data", toConstructorName: snakeCase }
instance ReadForeign HalfEnum where readImpl = readGenericTaggedSum halfEnumOptions
instance WriteForeign HalfEnum where writeImpl = writeGenericTaggedSum halfEnumOptions

data MyEnum = Enum1 | Enum2 | Enum3
derive instance Generic MyEnum _
derive instance Eq MyEnum
instance Show MyEnum where show = genericShow
instance ReadForeign MyEnum where readImpl = readGenericEnum
instance WriteForeign MyEnum where writeImpl = writeGenericEnum

data IntOrString = AnInt Int | AString String

derive instance Generic IntOrString _
derive instance Eq IntOrString
instance Show IntOrString where show = genericShow
instance ReadForeign IntOrString where readImpl = readGenericUntaggedSum
instance WriteForeign IntOrString where writeImpl = writeGenericUntaggedSum

data DoubleTrouble = IntAndString Int String

derive instance Generic DoubleTrouble _
derive instance Eq DoubleTrouble
instance Show DoubleTrouble where show = genericShow
instance ReadForeign DoubleTrouble where readImpl = readGenericUntaggedProduct
instance WriteForeign DoubleTrouble where writeImpl = writeGenericUntaggedProduct

data IntOrStringTagged = ATaggedInt Int | ATaggedString String

derive instance Generic IntOrStringTagged _
derive instance Eq IntOrStringTagged
instance Show IntOrStringTagged where show = genericShow
instance ReadForeign IntOrStringTagged where
  readImpl = readGenericTaggedSum defaultOptions
instance WriteForeign IntOrStringTagged where
  writeImpl = writeGenericTaggedSum defaultOptions
