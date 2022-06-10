module Yoga.JSON.Generics.TaggedSumRep where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (withExcept)
import Data.Generic.Rep as GR
import Foreign (Foreign, ForeignError(..), fail)
import Foreign as Foreign
import Yoga.JSON as JSON
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)

taggedSumRep
  :: forall a rep
   . GR.Generic a rep
  => GenericTaggedSumRep rep
  => Foreign
  -> Foreign.F a
taggedSumRep f = GR.to <$> genericTaggedSumRep f

-- | Generic Tagged Sum Representations, tagged with a "type" field
class GenericTaggedSumRep rep where
  genericTaggedSumRep :: Foreign -> Foreign.F rep

instance taggedSumRepSum ::
  ( GenericTaggedSumRep a
  , GenericTaggedSumRep b
  ) =>
  GenericTaggedSumRep (GR.Sum a b) where
  genericTaggedSumRep f = GR.Inl <$> genericTaggedSumRep f
    <|> GR.Inr <$> genericTaggedSumRep f

instance taggedSumRepConstructor ::
  ( GenericTaggedSumRep a
  , IsSymbol name
  ) =>
  GenericTaggedSumRep (GR.Constructor name a) where
  genericTaggedSumRep f = do
    r :: { "type" :: String, value :: Foreign } <- JSON.read' f
    if r."type" == name then withExcept (map $ ErrorAtProperty name) $
      GR.Constructor <$> genericTaggedSumRep r.value
    else
      fail $ ForeignError $ "Wrong type tag " <> r."type" <> " where " <> name
        <> " was expected."
    where
    nameP = Proxy :: Proxy name
    name = reflectSymbol nameP

instance taggedSumRepArgument ::
  ( JSON.ReadForeign a
  ) =>
  GenericTaggedSumRep (GR.Argument a) where
  genericTaggedSumRep f = GR.Argument <$> JSON.readImpl f
