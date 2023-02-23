module Yoga.JSON
  ( E
  , readJSON
  , readJSON'
  , readJSON_
  , writeJSON
  , writePrettyJSON
  , write
  , read
  , read'
  , read_
  , parseJSON
  , undefined
  , unsafeStringify
  , class ReadForeign
  , readImpl
  , class ReadForeignFields
  , getFields
  , class ReadForeignVariant
  , readVariantImpl
  , class ReadTuple
  , readTupleImpl
  , tupleSize
  , class WriteForeign
  , writeImpl
  , class WriteForeignFields
  , writeImplFields
  , class WriteForeignVariant
  , writeVariantImpl
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Control.Comonad.Cofree as Cofree
import Control.Monad.Except (ExceptT(..), except, runExcept, throwError, withExcept)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, fromArray, toArray)
import Data.Bifunctor (lmap)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Either (Either(..), hush, note)
import Data.Foldable (class Foldable, foldl)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Identity (Identity(..))
import Data.Int as Int
import Data.JSDate (JSDate)
import Data.JSDate as JSDate
import Data.List.NonEmpty (NonEmptyList, singleton)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Number as Number
import Data.String.NonEmpty.Internal (NonEmptyString)
import Data.String.NonEmpty.Internal as NonEmptyString
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Time.Duration (Days(..), Hours(..), Milliseconds(..), Minutes(..), Seconds(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj, on)
import Effect.Exception (message, try)
import Effect.Uncurried as EU
import Effect.Unsafe (unsafePerformEffect)
import Foreign (F, Foreign, ForeignError(..), MultipleErrors, fail, isNull, isUndefined, readArray, readBoolean, readChar, readInt, readNull, readNumber, readString, tagOf, unsafeFromForeign, unsafeReadTagged, unsafeToForeign)
import Foreign.Index (readProp)
import Foreign.Object (Object)
import Foreign.Object as Object
import JS.BigInt (BigInt)
import JS.BigInt (fromInt, fromNumber, fromString, toString) as BigInt
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Record (get)
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Tree (Tree)
import Yoga.Tree as Tree

-- | An alias for the Either result of decoding
type E a = Either MultipleErrors a

-- | Read a JSON string to a type `a` while returning a `MultipleErrors` if the
-- | parsing failed.
readJSON ∷
  ∀ a.
  ReadForeign a ⇒
  String →
  E a
readJSON = runExcept <<< (readImpl <=< parseJSON)

-- | Read a JSON string to a type `a` using `F a`. Useful with record types.
readJSON' ∷
  ∀ a.
  ReadForeign a ⇒
  String →
  F a
readJSON' = readImpl <=< parseJSON

-- | Read a JSON string to a type `a` while returning `Nothing` if the parsing
-- | failed.
readJSON_ ∷
  ∀ a.
  ReadForeign a ⇒
  String →
  Maybe a
readJSON_ = hush <<< readJSON

-- | JSON.stringify
foreign import _unsafeStringify ∷ ∀ a. a → String

-- | JSON.stringify with a number of spaces
foreign import _unsafePrettyStringify ∷ ∀ a. Int -> a → String

unsafeStringify ∷ ∀ a. a → String
unsafeStringify = _unsafeStringify

-- | Write a JSON string from a type `a`.
writeJSON ∷
  ∀ a.
  WriteForeign a ⇒
  a →
  String
writeJSON = _unsafeStringify <<< writeImpl

writePrettyJSON ∷
  ∀ a.
  WriteForeign a ⇒
  Int →
  a →
  String
writePrettyJSON spaces = _unsafePrettyStringify spaces <<< writeImpl

write ∷
  ∀ a.
  WriteForeign a ⇒
  a →
  Foreign
write = writeImpl

-- | Read a Foreign value to a type
read ∷
  ∀ a.
  ReadForeign a ⇒
  Foreign →
  E a
read = runExcept <<< readImpl

read' ∷
  ∀ a.
  ReadForeign a ⇒
  Foreign →
  F a
read' = readImpl

-- | Read a Foreign value to a type, as a Maybe of type
read_ ∷
  ∀ a.
  ReadForeign a ⇒
  Foreign →
  Maybe a
read_ = hush <<< read

foreign import _parseJSON ∷ EU.EffectFn1 String Foreign

parseJSON ∷ String → F Foreign
parseJSON = ExceptT
  <<< Identity
  <<< lmap (pure <<< ForeignError <<< message)
  <<< runPure
  <<< try
  <<< EU.runEffectFn1 _parseJSON
  where
  -- Nate Faubion: "It uses unsafePerformEffect because that’s the only way to catch exceptions and still use the builtin json decoder"
  runPure = unsafePerformEffect

foreign import _undefined ∷ Foreign

undefined ∷ Foreign
undefined = _undefined

-- | A class for reading foreign values to a type
class ReadForeign a where
  readImpl ∷ Foreign → F a

instance ReadForeign Foreign where
  readImpl = pure

instance ReadForeign Char where
  readImpl = readChar

instance ReadForeign Number where
  readImpl = readNumber

instance ReadForeign Int where
  readImpl = readInt

-- | Attempt to coerce a foreign value to a `BigInt`.
readBigInt ∷
  ∀ m. Monad m ⇒ Foreign → ExceptT (NonEmptyList ForeignError) m BigInt
readBigInt = unsafeReadTagged "BigInt"

instance ReadForeign BigInt where
  readImpl fValue = tryInt fValue <|> tryNumber fValue <|> readBigInt fValue <|> tryString fValue
    where
    tryInt f = readInt f <#> BigInt.fromInt
    tryNumber f = do
      num ← readNumber f
      if Number.round num == num then
        BigInt.fromNumber num
          # note (err $ "Cannot convert Number " <> show num <> " to BigInt")
          # except
      else
        (Left $ err $ "Cannot convert decimal Number " <> show num <> " to BigInt")
          # except
    tryString f = do
      bi ← readString f
      BigInt.fromString bi
        # note (err $ "Cannot convert String " <> bi <> " to BigInt")
        # except
    err = pure <<< ForeignError

instance ReadForeign String where
  readImpl = readString

instance ReadForeign NonEmptyString where
  readImpl = readString >=>
    NonEmptyString.fromString
      >>> note (pure $ ForeignError "String must not be empty")
      >>> except

instance ReadForeign Boolean where
  readImpl = readBoolean

instance ReadForeign a ⇒ ReadForeign (Array a) where
  readImpl = sequenceCombining <<< mapWithIndex readAtIdx <=< readArray

instance ReadForeign a ⇒ ReadForeign (Maybe a) where
  readImpl = readNullOrUndefined readImpl
    where
    readNullOrUndefined _ value | isNull value || isUndefined value = pure
      Nothing
    readNullOrUndefined f value = Just <$> f value

instance ReadForeign a ⇒ ReadForeign (Nullable a) where
  readImpl o = withExcept (map reformat) $
    map toNullable <$> traverse readImpl =<< readNull o
    where
    reformat error = case error of
      TypeMismatch inner other → TypeMismatch ("Nullable " <> inner) other
      _ → error

instance (ReadForeign a, ReadForeign b) ⇒ ReadForeign (Either a b) where
  readImpl f = do
    { type: tpe, value } ∷ { type ∷ String, value ∷ Foreign } ← readImpl f
    case tpe of
      "left" → Left <$> readImpl value
      "right" → Right <$> readImpl value
      _ → except $ Left (pure $ ForeignError $ "Invalid Either tag " <> tpe)

instance ReadForeign a ⇒ ReadForeign (Object.Object a) where
  readImpl = gatherErrors <<< Object.mapWithKey readProp <=< readObject'
    where
    gatherErrors ∷ Object (F a) → F (Object a)
    gatherErrors = Object.toUnfoldable
      >>> Array.foldl fn (Right (Object.empty))
      >>> except
      where
      fn ∷ _ → _ → (Either MultipleErrors (Object a))
      fn acc (Tuple k v) = do
        case acc, runExcept v of
          Left errs, Left errsNew → Left (errs <> errsNew)
          Left errs, Right _ → Left errs
          Right obj, Right value → Right (Object.insert k value obj)
          Right _, Left errs → Left errs

    readProp key value = except $ lmap (map (ErrorAtProperty key))
      (readImpl value # runExcept)

    readObject' ∷ Foreign → F (Object Foreign)
    readObject' value
      | tagOf value == "Object" = pure $ unsafeFromForeign value
      | otherwise = fail $ TypeMismatch "Object" (tagOf value)

instance ReadTuple (Tuple a b) ⇒ ReadForeign (Tuple a b) where
  readImpl = readTupleImpl 0

-- | A class for reading JSON arrays of lenth `n` as nested tuples of size `n`
class ReadTuple a where
  readTupleImpl ∷ Int → Foreign → F a
  tupleSize ∷ Proxy a → Int

instance
  ( ReadForeign a
  , ReadTuple (Tuple b c)
  ) ⇒
  ReadTuple (Tuple a (Tuple b c)) where
  readTupleImpl n =
    readImpl
      >=> case _ of
        arr → case Array.uncons arr of
          Just { head, tail } →
            lift2 Tuple
              (readAtIdx n head)
              (readTupleImpl (n + 1) $ writeImpl tail)
          _ → throwError $ pure $ TypeMismatch
            ( "array of length " <> show
                (1 + n + tupleSize (Proxy ∷ Proxy (Tuple b c)))
            )
            ("array of length " <> show n)
  tupleSize _ = 1 + tupleSize (Proxy ∷ Proxy (Tuple b c))
else instance readTupleHelper ∷
  ( ReadForeign a
  , ReadForeign b
  ) ⇒
  ReadTuple (Tuple a b) where
  readTupleImpl n =
    readImpl
      >=> case _ of
        [ a, b ] →
          lift2 Tuple (readAtIdx n a) (readAtIdx (n + 1) b)
        arr → throwError $ pure $ TypeMismatch
          ("array of length " <> show (n + 2))
          ("array of length " <> show (n + Array.length arr))

  tupleSize = const 2

instance
  ( RowToList fields fieldList
  , ReadForeignFields fieldList () fields
  ) ⇒
  ReadForeign (Record fields) where
  readImpl o = flip Builder.build {} <$> getFields fieldListP o
    where
    fieldListP = Proxy ∷ Proxy fieldList

-- | A class for reading foreign values from properties
class
  ReadForeignFields (xs ∷ RowList Type) (from ∷ Row Type) (to ∷ Row Type)
  | xs → from to where
  getFields ∷
    Proxy xs →
    Foreign →
    F (Builder (Record from) (Record to))

instance
  ( IsSymbol name
  , ReadForeign ty
  , ReadForeignFields tail from from'
  , Row.Lacks name from'
  , Row.Cons name ty from' to
  ) ⇒
  ReadForeignFields (Cons name ty tail) from to where
  getFields _ obj = except
    case runExcept first, runExcept rest of
      Right f, Right r → Right (f <<< r)
      Left e1, Left e2 → Left (e1 <> e2)
      Right _, Left es → Left es
      Left es, Right _ → Left es
    where
    value = enrichErrorWithPropName (readImpl =<< readProp name obj)
    first = Builder.insert nameP <$> value
    rest = getFields tailP obj
    nameP = Proxy ∷ Proxy name
    tailP = Proxy ∷ Proxy tail
    name = reflectSymbol nameP
    enrichErrorWithPropName = (withExcept <<< map) (ErrorAtProperty name)

readAtIdx ∷ ∀ a. ReadForeign a ⇒ Int → Foreign → F a
readAtIdx i f = withExcept (map (ErrorAtIndex i)) (readImpl f)

instance
  ReadForeignFields Nil () () where
  getFields _ _ =
    pure identity

instance
  ( RowToList variants rl
  , ReadForeignVariant rl variants
  ) ⇒
  ReadForeign (Variant variants) where
  readImpl o = readVariantImpl (Proxy ∷ Proxy rl) o

class
  ReadForeignVariant (xs ∷ RowList Type) (row ∷ Row Type)
  | xs → row where
  readVariantImpl ∷
    Proxy xs →
    Foreign →
    F (Variant row)

instance
  ( IsSymbol name
  , ReadForeign ty
  , Row.Cons name ty trash row
  , ReadForeignVariant tail row
  ) ⇒
  ReadForeignVariant (Cons name ty tail) row where
  readVariantImpl _ o = readVariantImpl (Proxy ∷ Proxy tail) o <|> ado
    value ∷ ty ← readProp name o >>= readImpl
    in inj namep value
    where
    namep = Proxy ∷ Proxy name
    name = reflectSymbol namep

instance
  ReadForeignVariant Nil trash where
  readVariantImpl _ _ = fail $ ForeignError
    "Unable to match any variant member."

-- -- | A class for writing a value into JSON
-- -- | need to do this intelligently using Foreign probably, because of null and undefined whatever
class WriteForeign a where
  writeImpl ∷ a → Foreign

instance WriteForeign Foreign where
  writeImpl = identity

instance WriteForeign String where
  writeImpl = unsafeToForeign

instance WriteForeign NonEmptyString where
  writeImpl = unsafeToForeign

instance WriteForeign Int where
  writeImpl = unsafeToForeign

instance WriteForeign Char where
  writeImpl = unsafeToForeign

instance WriteForeign Number where
  writeImpl = unsafeToForeign

instance WriteForeign BigInt where
  writeImpl = unsafeToForeign

instance WriteForeign Boolean where
  writeImpl = unsafeToForeign

instance WriteForeign a ⇒ WriteForeign (Array a) where
  writeImpl xs = unsafeToForeign $ writeImpl <$> xs

instance WriteForeign a ⇒ WriteForeign (Maybe a) where
  writeImpl = maybe undefined writeImpl

instance WriteForeign a ⇒ WriteForeign (Nullable a) where
  writeImpl = maybe (unsafeToForeign $ toNullable Nothing) writeImpl <<< toMaybe

instance (WriteForeign a, WriteForeign b) ⇒ WriteForeign (Either a b) where
  writeImpl value = case value of
    Left l → writeImpl { type: "left", value: writeImpl l }
    Right r → writeImpl { type: "right", value: writeImpl r }

instance WriteForeign a ⇒ WriteForeign (Object.Object a) where
  writeImpl = unsafeToForeign <<< Object.mapWithKey (const writeImpl)

instance
  ( WriteForeign a
  , WriteForeign (Tuple b c)
  ) ⇒
  WriteForeign (Tuple a (Tuple b c)) where
  writeImpl (Tuple a bc) =
    writeImpl bc
      # read_
      # fromMaybe []
      # Array.cons (writeImpl a)
      # writeImpl
else instance (WriteForeign a, WriteForeign b) ⇒ WriteForeign (Tuple a b) where
  writeImpl (Tuple a b) = writeImpl [ writeImpl a, writeImpl b ]

instance
  ( RowToList row rl
  , WriteForeignFields rl row () to
  ) ⇒
  WriteForeign (Record row) where
  writeImpl rec = unsafeToForeign $ Builder.build steps {}
    where
    rlp = Proxy ∷ Proxy rl
    steps = writeImplFields rlp rec

class
  WriteForeignFields (rl ∷ RowList Type) row (from ∷ Row Type) (to ∷ Row Type)
  | rl → row from to where
  writeImplFields ∷ ∀ g. g rl → Record row → Builder (Record from) (Record to)

instance
  ( IsSymbol name
  , WriteForeign ty
  , WriteForeignFields tail row from from'
  , Row.Cons name ty whatever row
  , Row.Lacks name from'
  , Row.Cons name Foreign from' to
  ) ⇒
  WriteForeignFields (Cons name ty tail) row from to where
  writeImplFields _ rec = result
    where
    namep = Proxy ∷ Proxy name
    value = writeImpl $ get namep rec
    tailp = Proxy ∷ Proxy tail
    rest = writeImplFields tailp rec
    result = Builder.insert namep value <<< rest

instance
  WriteForeignFields Nil row () () where
  writeImplFields _ _ = identity

instance
  ( RowToList row rl
  , WriteForeignVariant rl row
  ) ⇒
  WriteForeign (Variant row) where
  writeImpl variant = writeVariantImpl (Proxy ∷ Proxy rl) variant

class
  WriteForeignVariant (rl ∷ RowList Type) (row ∷ Row Type)
  | rl → row where
  writeVariantImpl ∷ ∀ g. g rl → Variant row → Foreign

instance
  WriteForeignVariant Nil () where
  writeVariantImpl _ _ =
    -- a PureScript-defined variant cannot reach this path, but a JavaScript FFI one could.
    unsafeCrashWith "Attempted to write empty variant."

instance
  ( IsSymbol name
  , WriteForeign ty
  , Row.Cons name ty subRow row
  , WriteForeignVariant tail subRow
  ) ⇒
  WriteForeignVariant (Cons name ty tail) row where
  writeVariantImpl _ variant =
    on
      namep
      writeVariant
      (writeVariantImpl (Proxy ∷ Proxy tail))
      variant
    where
    namep = Proxy ∷ Proxy name
    name = reflectSymbol namep
    writeVariant value = writeImpl $ Object.singleton name (writeImpl value)

instance ReadForeign a ⇒ ReadForeign (NonEmptyArray a) where
  readImpl f = do
    raw ∷ Array a ← readImpl f
    except
      $ note
          (singleton $ ForeignError "Nonempty array expected, got empty array")
      $ fromArray raw

instance writeForeignNEArray ∷ WriteForeign a ⇒ WriteForeign (NonEmptyArray a) where
  writeImpl a = writeImpl <<< toArray $ a

-- Map instances
instance (WriteForeign a) ⇒ WriteForeign (Map String a) where
  writeImpl = foldrWithIndex Object.insert Object.empty >>> writeImpl
else instance (WriteForeign a) ⇒ WriteForeign (Map Int a) where
  writeImpl = foldrWithIndex (show >>> Object.insert) Object.empty >>> writeImpl
else instance (WriteForeign a) ⇒ WriteForeign (Map BigInt a) where
  writeImpl = foldrWithIndex (BigInt.toString >>> Object.insert) Object.empty >>> writeImpl
else instance (Newtype nt key, WriteForeign (Map key value)) ⇒ WriteForeign (Map nt value) where
  writeImpl = (unsafeCoerce ∷ (_ → Map key value)) >>> writeImpl

instance (ReadForeign a) ⇒ ReadForeign (Map String a) where
  readImpl = (readImpl ∷ (_ → _ (Object a))) >>> map (foldrWithIndex Map.insert Map.empty)
else instance (ReadForeign a) ⇒ ReadForeign (Map Int a) where
  readImpl = (readImpl ∷ (_ → _ (Object a))) >>> map (foldrWithIndex (unsafeStringToInt >>> Map.insert) Map.empty)
else instance (ReadForeign a) ⇒ ReadForeign (Map BigInt a) where
  readImpl = (readImpl ∷ (_ → _ (Object a))) >>> map (foldrWithIndex (unsafeStringToBigInt >>> Map.insert) Map.empty)
else instance (Newtype nt key, ReadForeign (Map key value)) ⇒ ReadForeign (Map nt value) where
  readImpl = (readImpl ∷ (_ → _ (Map key value))) >>> map (unsafeCoerce ∷ (Map key value → Map nt value))

-- Date instances
instance WriteForeign JSDate where
  writeImpl = JSDate.toISOString >>> unsafePerformEffect >>> writeImpl

instance ReadForeign JSDate where
  readImpl = readImpl >>> map (JSDate.parse >>> unsafePerformEffect)

instance WriteForeign DateTime where
  writeImpl = JSDate.fromDateTime >>> writeImpl

instance ReadForeign DateTime where
  readImpl = readImpl >=>
    JSDate.toDateTime
      >>> note (pure $ ForeignError "Invalid date time")
      >>> except

instance WriteForeign Instant where
  writeImpl = unInstant >>> writeImpl

instance ReadForeign Instant where
  readImpl f = do
    millis ← readImpl f
    case instant millis of
      Nothing → except $ Left (pure $ ForeignError "Invalid instant")
      Just ins → pure ins

instance WriteForeign Milliseconds where
  writeImpl = unwrap >>> writeImpl

instance ReadForeign Milliseconds where
  readImpl = readImpl >>> map wrap

instance WriteForeign Seconds where
  writeImpl = unwrap >>> writeImpl

instance ReadForeign Seconds where
  readImpl = readImpl >>> map wrap

instance WriteForeign Minutes where
  writeImpl = unwrap >>> writeImpl

instance ReadForeign Minutes where
  readImpl = readImpl >>> map wrap

instance WriteForeign Hours where
  writeImpl = unwrap >>> writeImpl

instance ReadForeign Hours where
  readImpl = readImpl >>> map wrap

instance WriteForeign Days where
  writeImpl = unwrap >>> writeImpl

instance ReadForeign Days where
  readImpl = readImpl >>> map wrap

unsafeStringToInt ∷ String → Int
unsafeStringToInt = Int.fromString >>>
  (fromMaybe' \_ → unsafeCrashWith "impossible")

unsafeStringToBigInt ∷ String → BigInt
unsafeStringToBigInt = BigInt.fromString >>>
  (fromMaybe' \_ → unsafeCrashWith "impossible")

sequenceCombining ∷
  ∀ f a.
  Monoid (f a) ⇒
  Foldable f ⇒
  Applicative f ⇒
  f (F a) →
  F (f a)
sequenceCombining = foldl fn (Right mempty) >>> except
  where
  fn ∷ _ → _ → (Either MultipleErrors (f a))
  fn acc elem = do
    case acc, runExcept elem of
      Left errs, Left errsNew → Left (errs <> errsNew)
      Left errs, Right _ → Left errs
      Right values, Right value → Right (values <> pure value)
      Right _, Left errs → Left errs

instance WriteForeign a ⇒ WriteForeign (Tree a) where
  writeImpl ∷ Tree a → Foreign
  writeImpl t = write { value: Cofree.head t, children }
    where
    tail = Cofree.tail t
    children = if Array.null tail then Nothing else Just (writeImpl <$> tail)

instance ReadForeign a ⇒ ReadForeign (Tree a) where
  readImpl f = do
    { value, children } ∷ { value ∷ a, children ∷ Maybe (Array Foreign) } ← readImpl f
    case children of
      Nothing → pure (Tree.leaf value)
      Just cs → traverse readImpl cs <#> Tree.mkTree value
