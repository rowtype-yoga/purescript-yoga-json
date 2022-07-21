module Test.Util where

import Prelude

import Data.Either (Either(..))
import Data.Semigroup.Foldable (intercalateMap)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Test.Spec.Assertions (fail)
import Type.Proxy (Proxy(..))
import Yoga.JSON (class ReadForeign, class WriteForeign, read, readJSON, write, writeJSON)
import Yoga.JSON.Error (renderHumanError, toJSONPath)

roundtrips ∷ ∀ a. Show a ⇒ Eq a ⇒ ReadForeign a ⇒ WriteForeign a ⇒ a → Aff Unit
roundtrips x = do
  x # write # shouldRead (Proxy ∷ _ a)
  x # writeJSON # shouldReadJSON (Proxy ∷ _ a)

shouldRead ∷ ∀ a. ReadForeign a ⇒ Proxy a → Foreign → Aff Unit
shouldRead _ = read >>> case _ of
  Left e → fail (intercalateMap "\n" renderHumanError e <> "\n" <> intercalateMap "\n" toJSONPath e)
  Right (_ ∷ a) → pure unit

shouldReadJSON ∷ ∀ a. ReadForeign a ⇒ Proxy a → String → Aff Unit
shouldReadJSON _ = readJSON >>> case _ of
  Left e → fail (intercalateMap "\n" renderHumanError e <> "\n" <> intercalateMap "\n" toJSONPath e)
  Right (_ ∷ a) → pure unit
