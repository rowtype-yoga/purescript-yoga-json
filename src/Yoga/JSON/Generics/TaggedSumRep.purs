module Yoga.JSON.Generics.TaggedSumRep where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (withExcept)
import Data.Generic.Rep (NoArguments)
import Data.Generic.Rep as GR
import Data.Maybe (maybe)
import Data.Tuple.Nested ((/\))
import Foreign (Foreign, ForeignError(..), fail)
import Foreign as Foreign
import Foreign.Object (Object)
import Foreign.Object as Object
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import Yoga.JSON (undefined)
import Yoga.JSON as JSON

type Options =
  { typeTag ∷ String, valueTag ∷ String, toConstructorName ∷ String → String }

defaultOptions ∷ Options
defaultOptions = { typeTag: "type", valueTag: "value", toConstructorName: identity }

genericReadForeignTaggedSum ∷
  ∀ a rep.
  GR.Generic a rep ⇒
  ReadGenericTaggedSumRep rep ⇒
  Options →
  Foreign →
  Foreign.F a
genericReadForeignTaggedSum options f =
  GR.to <$> genericReadForeignTaggedSumRep options f

-- | Generic Tagged Sum Representations, tagged with a "type" field
class ReadGenericTaggedSumRep rep where
  genericReadForeignTaggedSumRep ∷ Options → Foreign → Foreign.F rep

instance
  ( ReadGenericTaggedSumRep a
  , ReadGenericTaggedSumRep b
  ) ⇒
  ReadGenericTaggedSumRep (GR.Sum a b) where
  genericReadForeignTaggedSumRep options f = GR.Inl <$> genericReadForeignTaggedSumRep options f
    <|> GR.Inr <$> genericReadForeignTaggedSumRep options f

else instance
  ( IsSymbol name
  ) ⇒
  ReadGenericTaggedSumRep (GR.Constructor name NoArguments) where
  genericReadForeignTaggedSumRep { typeTag, toConstructorName } f = do
    o ∷ Object Foreign ← JSON.read' f
    typeFgn ← maybe (fail ((ErrorAtProperty typeTag) (ForeignError ("Missing type tag: " <> typeTag)))) pure (Object.lookup typeTag o)
    typeStr ← JSON.read' typeFgn
    if typeStr == name then withExcept (map $ ErrorAtProperty name) $
      pure (GR.Constructor GR.NoArguments)
    else
      fail $ ForeignError $ "Wrong type tag " <> typeStr <> " where " <> typeTag
        <> " was expected."
    where
    nameP = Proxy ∷ Proxy name
    name = toConstructorName (reflectSymbol nameP)
else instance
  ( ReadGenericTaggedSumRep a
  , IsSymbol name
  ) ⇒
  ReadGenericTaggedSumRep (GR.Constructor name a) where
  genericReadForeignTaggedSumRep options@{ typeTag, valueTag, toConstructorName } f = do
    o ∷ Object Foreign ← JSON.read' f
    typeFgn ← maybe (fail ((ErrorAtProperty typeTag) (ForeignError ("Missing type tag: " <> typeTag)))) pure (Object.lookup typeTag o)
    typeStr ← JSON.read' typeFgn
    value ← maybe (fail ((ErrorAtProperty valueTag) (ForeignError ("Missing value tag: " <> valueTag)))) pure (Object.lookup valueTag o)
    if typeStr == name then withExcept (map $ ErrorAtProperty name) $
      GR.Constructor <$> genericReadForeignTaggedSumRep options value
    else
      fail $ ForeignError $ "Wrong constructor name tag " <> typeStr <> " where " <> name
        <> " was expected."
    where
    nameP = Proxy ∷ Proxy name
    name = toConstructorName (reflectSymbol nameP)

instance (JSON.ReadForeign a) ⇒ ReadGenericTaggedSumRep (GR.Argument a) where
  genericReadForeignTaggedSumRep _ f = GR.Argument <$> JSON.readImpl f

-- Write

genericWriteForeignTaggedSum ∷
  ∀ a rep.
  GR.Generic a rep ⇒
  WriteGenericTaggedSumRep rep ⇒
  Options →
  a →
  Foreign
genericWriteForeignTaggedSum options r =
  genericWriteForeignTaggedSumRep options (GR.from r)

-- | Generic Tagged Sum Representations, tagged with a "type" field
class WriteGenericTaggedSumRep rep where
  genericWriteForeignTaggedSumRep ∷ Options → rep → Foreign

instance
  ( WriteGenericTaggedSumRep a
  , WriteGenericTaggedSumRep b
  ) ⇒
  WriteGenericTaggedSumRep (GR.Sum a b) where
  genericWriteForeignTaggedSumRep options = case _ of
    GR.Inl a → genericWriteForeignTaggedSumRep options a
    GR.Inr b → genericWriteForeignTaggedSumRep options b

instance
  ( WriteGenericTaggedSumRep a
  , IsSymbol name
  ) ⇒
  WriteGenericTaggedSumRep (GR.Constructor name a) where
  genericWriteForeignTaggedSumRep options@{ typeTag, valueTag, toConstructorName } (GR.Constructor a) = do
    JSON.write
      ( Object.fromFoldable
          [ typeTag /\ JSON.write name
          , valueTag /\ genericWriteForeignTaggedSumRep options a
          ]
      )
    where
    nameP = Proxy ∷ Proxy name
    name = toConstructorName (reflectSymbol nameP)

instance (JSON.WriteForeign a) ⇒ WriteGenericTaggedSumRep (GR.Argument a) where
  genericWriteForeignTaggedSumRep _ (GR.Argument a) = JSON.writeImpl a

instance
  WriteGenericTaggedSumRep GR.NoArguments where
  genericWriteForeignTaggedSumRep _ GR.NoArguments = undefined