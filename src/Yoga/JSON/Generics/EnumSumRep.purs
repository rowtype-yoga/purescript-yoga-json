module Yoga.JSON.Generics.EnumSumRep where

import Prelude

import Control.Alt ((<|>))
import Data.Generic.Rep as GR
import Foreign (Foreign, fail)
import Foreign as Foreign
import Yoga.JSON as JSON
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)

genericReadForeignEnum
  :: forall a rep
   . GR.Generic a rep
  => GenericEnumSumRep rep
  => Foreign
  -> Foreign.F a
genericReadForeignEnum f =
  GR.to <$> genericEnumReadForeign f

genericWriteForeignEnum
  :: forall a rep
   . GR.Generic a rep
  => GenericEnumSumRep rep
  => a
  -> Foreign
genericWriteForeignEnum a = genericEnumWriteForeign (GR.from a)

-- | Generic Enum Sum Representations, with constructor names as strings
class GenericEnumSumRep rep where
  genericEnumReadForeign :: Foreign -> Foreign.F rep
  genericEnumWriteForeign :: rep -> Foreign

instance ( GenericEnumSumRep a , GenericEnumSumRep b) =>
  GenericEnumSumRep (GR.Sum a b) where
  genericEnumReadForeign f = GR.Inl <$> genericEnumReadForeign f
    <|> GR.Inr <$> genericEnumReadForeign f
  genericEnumWriteForeign = case _ of
   (GR.Inl a) -> genericEnumWriteForeign a
   (GR.Inr a) -> genericEnumWriteForeign a

instance (IsSymbol name) =>
  GenericEnumSumRep (GR.Constructor name GR.NoArguments) where
  genericEnumReadForeign f = do
    s <- JSON.readImpl f
    if s == name then pure $ GR.Constructor GR.NoArguments
    else fail <<< Foreign.ForeignError $
      "Enum string " <> s <> " did not match expected string " <> name
    where
    name = reflectSymbol (Proxy :: Proxy name)
  genericEnumWriteForeign (GR.Constructor GR.NoArguments) =
    JSON.writeImpl $ reflectSymbol (Proxy :: Proxy name)
