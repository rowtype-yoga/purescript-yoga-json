module Yoga.JSON.Generics.EnumSumRep where

import Prelude

import Control.Alt ((<|>))
import Data.Generic.Rep as GR
import Foreign (Foreign, fail)
import Foreign as Foreign
import Yoga.JSON as JSON
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)

enumSumRep
  :: forall a rep
   . GR.Generic a rep
  => GenericEnumSumRep rep
  => Foreign
  -> Foreign.F a
enumSumRep f =
  GR.to <$> genericEnumReadForeign f

-- | Generic Enum Sum Representations, with constructor names as strings
class GenericEnumSumRep rep where
  genericEnumReadForeign :: Foreign -> Foreign.F rep

instance sumEnumSumRep ::
  ( GenericEnumSumRep a
  , GenericEnumSumRep b
  ) =>
  GenericEnumSumRep (GR.Sum a b) where
  genericEnumReadForeign f = GR.Inl <$> genericEnumReadForeign f
    <|> GR.Inr <$> genericEnumReadForeign f

instance constructorEnumSumRep ::
  ( IsSymbol name
  ) =>
  GenericEnumSumRep (GR.Constructor name GR.NoArguments) where
  genericEnumReadForeign f = do
    s <- JSON.readImpl f
    if s == name then pure $ GR.Constructor GR.NoArguments
    else fail <<< Foreign.ForeignError $
      "Enum string " <> s <> " did not match expected string " <> name
    where
    name = reflectSymbol (Proxy :: Proxy name)
