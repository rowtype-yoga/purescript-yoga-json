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

readGenericTaggedSum ∷
  ∀ a rep.
  GR.Generic a rep ⇒
  ReadGenericTaggedSumRep rep ⇒
  Options →
  Foreign →
  Foreign.F a
readGenericTaggedSum options f =
  GR.to <$> readGenericTaggedSumRep options f

-- | Generic Tagged Sum Representations, tagged with a "type" field
class ReadGenericTaggedSumRep rep where
  readGenericTaggedSumRep ∷ Options → Foreign → Foreign.F rep

instance
  ( ReadGenericTaggedSumRep a
  , ReadGenericTaggedSumRep b
  ) ⇒
  ReadGenericTaggedSumRep (GR.Sum a b) where
  readGenericTaggedSumRep options f = GR.Inl <$> readGenericTaggedSumRep options f
    <|> GR.Inr <$> readGenericTaggedSumRep options f

else instance
  ( IsSymbol name
  ) ⇒
  ReadGenericTaggedSumRep (GR.Constructor name NoArguments) where
  readGenericTaggedSumRep { typeTag, toConstructorName } f = do
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
  readGenericTaggedSumRep options@{ typeTag, valueTag, toConstructorName } f = do
    o ∷ Object Foreign ← JSON.read' f
    typeFgn ← maybe (fail ((ErrorAtProperty typeTag) (ForeignError ("Missing type tag: " <> typeTag)))) pure (Object.lookup typeTag o)
    typeStr ← JSON.read' typeFgn
    value ← maybe (fail ((ErrorAtProperty valueTag) (ForeignError ("Missing value tag: " <> valueTag)))) pure (Object.lookup valueTag o)
    if typeStr == name then withExcept (map $ ErrorAtProperty name) $
      GR.Constructor <$> readGenericTaggedSumRep options value
    else
      fail $ ForeignError $ "Wrong constructor name tag " <> typeStr <> " where " <> name
        <> " was expected."
    where
    nameP = Proxy ∷ Proxy name
    name = toConstructorName (reflectSymbol nameP)

instance (JSON.ReadForeign a) ⇒ ReadGenericTaggedSumRep (GR.Argument a) where
  readGenericTaggedSumRep _ f = GR.Argument <$> JSON.readImpl f

-- Write

writeGenericTaggedSum ∷
  ∀ a rep.
  GR.Generic a rep ⇒
  WriteGenericTaggedSumRep rep ⇒
  Options →
  a →
  Foreign
writeGenericTaggedSum options r =
  writeGenericTaggedSumRep options (GR.from r)

-- | Generic Tagged Sum Representations, tagged with a "type" field
class WriteGenericTaggedSumRep rep where
  writeGenericTaggedSumRep ∷ Options → rep → Foreign

instance
  ( WriteGenericTaggedSumRep a
  , WriteGenericTaggedSumRep b
  ) ⇒
  WriteGenericTaggedSumRep (GR.Sum a b) where
  writeGenericTaggedSumRep options = case _ of
    GR.Inl a → writeGenericTaggedSumRep options a
    GR.Inr b → writeGenericTaggedSumRep options b

instance
  ( WriteGenericTaggedSumRep a
  , IsSymbol name
  ) ⇒
  WriteGenericTaggedSumRep (GR.Constructor name a) where
  writeGenericTaggedSumRep options@{ typeTag, valueTag, toConstructorName } (GR.Constructor a) = do
    JSON.write
      ( Object.fromFoldable
          [ typeTag /\ JSON.write name
          , valueTag /\ writeGenericTaggedSumRep options a
          ]
      )
    where
    nameP = Proxy ∷ Proxy name
    name = toConstructorName (reflectSymbol nameP)

instance (JSON.WriteForeign a) ⇒ WriteGenericTaggedSumRep (GR.Argument a) where
  writeGenericTaggedSumRep _ (GR.Argument a) = JSON.writeImpl a

instance
  WriteGenericTaggedSumRep GR.NoArguments where
  writeGenericTaggedSumRep _ GR.NoArguments = undefined