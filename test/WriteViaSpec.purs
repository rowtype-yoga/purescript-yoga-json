module Test.WriteViaSpec where

import Prelude

import Data.Either (Either(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Yoga.JSON (class ReadForeign, class WriteForeign)
import Yoga.JSON as JSON
import Yoga.JSON.Derive (readVia, writeVia)

spec ∷ Spec Unit
spec = describe "En- and decoding with Coercibles" do
  let referenceJSON = """{"myString":"A","myInt":3}"""
  it "writing works" do
    let myRecord = MyRecord { myInt: MyInt 3, myString: MyString "A" }
    JSON.writeJSON myRecord `shouldEqual` referenceJSON
  it "reading works" do
    JSON.readJSON referenceJSON `shouldEqual` Right (MyRecord { myInt: MyInt 3, myString: MyString "A" })

newtype MyInt = MyInt Int
newtype MyString = MyString String

newtype MyRecord = MyRecord
  { myInt ∷ MyInt
  , myString ∷ MyString
  }

instance WriteForeign MyRecord where
  writeImpl = writeVia @{ myInt ∷ Int, myString ∷ String }

instance ReadForeign MyRecord where
  readImpl = readVia @{ myInt ∷ Int, myString ∷ String }

-- Below instances are just necessary for the tests to compile

instance Show MyRecord where
  show = JSON.writeJSON

derive instance Eq MyRecord
derive instance Eq MyInt
derive instance Eq MyString
