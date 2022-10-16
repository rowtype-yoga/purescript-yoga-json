module Test.ErrorsSpec where

import Prelude

import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), blush)
import Data.Maybe (Maybe(..))
import Foreign (ForeignError(..))
import Foreign.Object (Object)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))
import Yoga.JSON (class ReadForeign, E, readJSON)
import Yoga.JSON as JSON
import Yoga.JSON.Error (renderHumanError, toJSONPath)

spec ∷ Spec Unit
spec = describe "Errors" do
  describe "are returned at once" do
    it "returns multiple errors for object" do
      let
        res ∷ E (Object Int)
        res = JSON.readJSON """{ "something": "no", "else": true }"""
        e1 = ErrorAtProperty "something" (TypeMismatch "Int" "String")
        e2 = ErrorAtProperty "else" (TypeMismatch "Int" "Boolean")
      res `shouldEqual` (Left (pure e1 <> pure e2))

    it "returns multiple errors for record" do
      let
        res ∷ E { a ∷ Int, b ∷ String }
        res = JSON.readJSON """{ "b": true }"""
        e1 = ErrorAtProperty "a" (TypeMismatch "Int" "Undefined")
        e2 = ErrorAtProperty "b" (TypeMismatch "String" "Boolean")
      res `shouldEqual` (Left (pure e1 <> pure e2))

    it "returns multiple errors for array" do
      let
        res ∷ E (Array Int)
        res = JSON.readJSON """[1,"a",2,true]"""
        e1 = ErrorAtIndex 1 (TypeMismatch "Int" "String")
        e2 = ErrorAtIndex 3 (TypeMismatch "Int" "Boolean")
      res `shouldEqual` (Left (pure e1 <> pure e2))

  describe "produces human-friendly errors" do
    it "For some object" do
      let
        res ∷ E (Object Int)
        res = JSON.readJSON """{ "something": "no", "else": true }"""
        e1 = "Must provide a value of type 'Int' instead of 'String' at $.something"
        e2 = "Must provide a value of type 'Int' instead of 'Boolean' at $.else"
      (res # lmap (map renderHumanError >>> Array.fromFoldable))
        `shouldEqual` (Left [ e1, e2 ])

    it "For deeply nested keys" do
      let
        res ∷ E { a ∷ { b ∷ { c ∷ { d ∷ String } } } }
        res = JSON.readJSON """{ "a": { "b": { "c": { }}}}"""
        e = "Must provide a value of type 'String' at $.a.b.c.d"
      (res # lmap (map renderHumanError >>> Array.fromFoldable))
        `shouldEqual` (Left [ e ])

  describe "have the correct JSON path" do
    it "empty json" do
      (getErrorPath (Proxy ∷ _ {}) "") `shouldEqual` Just [ "$" ]
    it "nested json json" do
      let
        xo =
          """
          { "deeply": { "nested": { "array":
            [ { "of": { "values": "hello" } }
              , 8
            ]
          } } }"""
      ( getErrorPath (Proxy ∷ _ { deeply ∷ { nested ∷ { array ∷ Array { of ∷ { values ∷ String } } } } })
          xo
      ) `shouldEqual`
        -- [FIXME] I'm not sure about this
        Just [ "$.deeply.nested.array[1].of" ]

getErrorPath ∷ ∀ a. ReadForeign a ⇒ Proxy a → String → Maybe (Array String)
getErrorPath _ x = do
  let e = (readJSON x ∷ _ a) # blush
  (e <#> map toJSONPath <#> Array.fromFoldable)
