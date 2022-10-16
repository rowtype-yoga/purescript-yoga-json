module Yoga.JSON.Variant where

import Prelude

import Control.Alt ((<|>))
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant, inj, on)
import Foreign (F, Foreign, ForeignError(..), fail)
import Foreign.Index (readProp)
import Foreign.Object as Object
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Type.Proxy (Proxy(..))
import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

newtype TaggedVariant ∷ Symbol → Symbol → Row Type → Type
newtype TaggedVariant tt vt v = TaggedVariant (Variant v)

instance Newtype (TaggedVariant tt vt v) (Variant v)
derive newtype instance (Show (Variant v)) ⇒ Show (TaggedVariant tt vt v)
derive newtype instance (Eq (Variant v)) ⇒ Eq (TaggedVariant tt vt v)

instance
  ( RowToList row rl
  , WriteForeignTaggedVariant rl row
  , IsSymbol typeTag
  , IsSymbol valueTag
  ) ⇒
  WriteForeign (TaggedVariant typeTag valueTag row) where
  writeImpl (TaggedVariant variant) =
    writeVariantImpl
      (reflectSymbol (Proxy ∷ Proxy typeTag))
      (reflectSymbol (Proxy ∷ Proxy valueTag))
      (Proxy ∷ Proxy rl)
      variant

class
  WriteForeignTaggedVariant (rl ∷ RowList Type) (row ∷ Row Type)
  | rl → row where
  writeVariantImpl ∷ ∀ g. String → String → g rl → Variant row → Foreign

instance
  WriteForeignTaggedVariant Nil () where
  writeVariantImpl _ _ _ _ =
    -- a PureScript-defined variant cannot reach this path, but a JavaScript FFI one could.
    unsafeCrashWith "Attempted to write empty Variant"

instance
  ( IsSymbol name
  , WriteForeign ty
  , Row.Cons name ty subRow row
  , WriteForeignTaggedVariant tail subRow
  ) ⇒
  WriteForeignTaggedVariant (Cons name ty tail) row where
  writeVariantImpl typeTag valueTag _ variant =
    on
      namep
      writeVariant
      (writeVariantImpl typeTag valueTag (Proxy ∷ Proxy tail))
      variant
    where
    namep = Proxy ∷ Proxy name
    name = reflectSymbol namep
    writeVariant value = writeImpl $
      Object.fromFoldable
        [ valueTag /\ (writeImpl value)
        , typeTag /\ (writeImpl name)
        ]

instance
  ( RowToList variants rl
  , ReadForeignTaggedVariant typeTag valueTag rl variants
  , IsSymbol typeTag
  , IsSymbol valueTag
  ) ⇒
  ReadForeign (TaggedVariant typeTag valueTag variants) where
  readImpl o = readVariantImpl (Proxy ∷ Proxy rl) o

class
  ReadForeignTaggedVariant typeTag valueTag (xs ∷ RowList Type) (row ∷ Row Type)
  | xs → row where
  readVariantImpl ∷
    Proxy xs →
    Foreign →
    F (TaggedVariant typeTag valueTag row)

instance
  ReadForeignTaggedVariant typeTag valueTag Nil trash where
  readVariantImpl __ _ = fail $ ForeignError
    "Unable to match any variant member."

instance
  ( IsSymbol name
  , IsSymbol typeTag
  , IsSymbol valueTag
  , ReadForeign ty
  , Row.Cons name ty trash row
  , ReadForeignTaggedVariant typeTag valueTag tail row
  ) ⇒
  ReadForeignTaggedVariant typeTag valueTag (Cons name ty tail) row where
  readVariantImpl _ o = readVariantImpl (Proxy ∷ Proxy tail) o <|> do
    type_ ← readProp typeTag o >>= readImpl
    if type_ == name then do
      value ∷ ty ← readProp valueTag o >>= readImpl
      pure $ TaggedVariant (inj namep value)
    else
      (fail <<< ForeignError $ "Did not match variant tag " <> name)

    where
    typeTag = reflectSymbol (Proxy ∷ Proxy typeTag)
    valueTag = reflectSymbol (Proxy ∷ Proxy valueTag)
    namep = Proxy ∷ Proxy name
    name = reflectSymbol namep

-- Untagged Variant

newtype UntaggedVariant ∷ Row Type → Type
newtype UntaggedVariant v = UntaggedVariant (Variant v)

instance Newtype (UntaggedVariant v) (Variant v)
derive newtype instance (Show (Variant v)) ⇒ Show (UntaggedVariant v)
derive newtype instance (Eq (Variant v)) ⇒ Eq (UntaggedVariant v)

instance
  ( RowToList row rl
  , WriteForeignUntaggedVariant rl row
  ) ⇒
  WriteForeign (UntaggedVariant row) where
  writeImpl (UntaggedVariant variant) =
    writeUntaggedVariantImpl
      (Proxy ∷ Proxy rl)
      variant

class
  WriteForeignUntaggedVariant (rl ∷ RowList Type) (row ∷ Row Type)
  | rl → row where
  writeUntaggedVariantImpl ∷ ∀ g. g rl → Variant row → Foreign

instance
  WriteForeignUntaggedVariant Nil () where
  writeUntaggedVariantImpl _ _ =
    -- a PureScript-defined variant cannot reach this path, but a JavaScript FFI one could.
    unsafeCrashWith "Attempted to write empty Variant"

instance
  ( IsSymbol name
  , WriteForeign ty
  , Row.Cons name ty subRow row
  , WriteForeignUntaggedVariant tail subRow
  ) ⇒
  WriteForeignUntaggedVariant (Cons name ty tail) row where
  writeUntaggedVariantImpl _ variant =
    on
      namep
      writeImpl
      (writeUntaggedVariantImpl (Proxy ∷ Proxy tail))
      variant
    where
    namep = Proxy ∷ Proxy name

instance
  ( RowToList variants rl
  , ReadForeignUntaggedVariant rl variants
  ) ⇒
  ReadForeign (UntaggedVariant variants) where
  readImpl o = readUntaggedVariantImpl (Proxy ∷ Proxy rl) o

class
  ReadForeignUntaggedVariant (xs ∷ RowList Type) (row ∷ Row Type)
  | xs → row where
  readUntaggedVariantImpl ∷
    Proxy xs →
    Foreign →
    F (UntaggedVariant row)

instance
  ReadForeignUntaggedVariant Nil trash where
  readUntaggedVariantImpl _ _ = fail $ ForeignError
    "Unable to match any variant member."

instance
  ( IsSymbol name
  , ReadForeign ty
  , Row.Cons name ty trash row
  , ReadForeignUntaggedVariant tail row
  ) ⇒
  ReadForeignUntaggedVariant (Cons name ty tail) row where
  readUntaggedVariantImpl _ o =
    readUntaggedVariantImpl (Proxy ∷ Proxy tail) o <|> ado
      v ← readImpl o
      in UntaggedVariant (inj namep v)

    where
    namep = Proxy ∷ Proxy name