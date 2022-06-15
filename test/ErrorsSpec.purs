module Test.ErrorsSpec where

import Prelude

import Data.Array as Array
import Data.Either (blush)
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))
import Yoga.JSON (class ReadForeign, readJSON)
import Yoga.JSON.Error (toJSONPath)

spec ∷ Spec Unit
spec = describe "Errors" do
  describe "have the correct JSON path" do
    it "empty json" do
      (getErrorPath (Proxy ∷ _ {}) "") `shouldEqual` Just [ "$" ]
    it "nested json json" do
      let
        xo ="""
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