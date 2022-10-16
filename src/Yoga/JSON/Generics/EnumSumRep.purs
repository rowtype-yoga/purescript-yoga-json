module Yoga.JSON.Generics.EnumSumRep where

import Prelude

import Control.Alt ((<|>))
import Data.Generic.Rep as GR
import Foreign (Foreign, fail)
import Foreign as Foreign
import Yoga.JSON as JSON
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)

type Options =
  { toConstructorName ∷ String → String }

defaultOptions ∷ Options
defaultOptions = { toConstructorName: identity }

genericReadForeignEnum ∷
  ∀ a rep.
  GR.Generic a rep ⇒
  GenericEnumSumRep rep ⇒
  Options →
  Foreign →
  Foreign.F a
genericReadForeignEnum options f =
  GR.to <$> genericEnumReadForeign options f

genericWriteForeignEnum ∷
  ∀ a rep.
  GR.Generic a rep ⇒
  GenericEnumSumRep rep ⇒
  Options →
  a →
  Foreign
genericWriteForeignEnum options a = genericEnumWriteForeign options (GR.from a)

-- | Generic Enum Sum Representations, with constructor names as strings
class GenericEnumSumRep rep where
  genericEnumReadForeign ∷ Options → Foreign → Foreign.F rep
  genericEnumWriteForeign ∷ Options → rep → Foreign

instance
  ( GenericEnumSumRep a
  , GenericEnumSumRep b
  ) ⇒
  GenericEnumSumRep (GR.Sum a b) where
  genericEnumReadForeign options f = GR.Inl <$> genericEnumReadForeign options f
    <|> GR.Inr <$> genericEnumReadForeign options f
  genericEnumWriteForeign options = case _ of
    (GR.Inl a) → genericEnumWriteForeign options a
    (GR.Inr a) → genericEnumWriteForeign options a

instance
  ( IsSymbol name
  ) ⇒
  GenericEnumSumRep (GR.Constructor name GR.NoArguments) where
  genericEnumReadForeign options f = do
    s ← JSON.readImpl f
    if s == options.toConstructorName name then pure $ GR.Constructor GR.NoArguments
    else fail <<< Foreign.ForeignError $
      "Enum string " <> s <> " did not match expected string " <> options.toConstructorName name
    where
    name = reflectSymbol (Proxy ∷ Proxy name)
  genericEnumWriteForeign options (GR.Constructor GR.NoArguments) =
    JSON.writeImpl $ options.toConstructorName (reflectSymbol (Proxy ∷ Proxy name))
