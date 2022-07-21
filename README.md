# 🐏👑 purescript-yoga-json

**Note**: This is a fork of [simple-json](https://github.com/justinwoo/purescript-simple-json) ([MIT Licence](./LICENSE/simple-json.LICENSE)).

## Table of Contents
* [usage](#usage)
* [migrate from `simple-json`](#migrate-from-purescript-simple-json)

## Usage

```purescript
import Yoga.JSON as JSON

serialised :: String
serialised =
  JSON.writeJSON { first_name: "Lola", last_name: "Flores" }
```

Check out the tests for how to encode/decode increasingly complex types.

## Migrate from `purescript-simple-json`

`purescript-yoga-json` is a drop-in replacement for `purescript-simple-json`. Just change the imports from `Simple.JSON` to `Yoga.JSON`.

## Differences to `simple-json`

### Tuples
There is an inbuilt codec for `Tuple`s thanks to @ursi

### Generics
It includes @justinwoo's codecs for en- and decoding generics inspired by
[simple-json-generics](https://github.com/justinwoo/purescript-simple-json-generics)

It is possible to customise the representation of enums, tagged sum types, and untagged sum types via options.

### BigInts
It can *read* bigints (if you install `big-integer` as a JS dependency).

### 💣 Cannot write bigints as bigints but only strings
It seems that there is no way to write bigints in JavaScript except for writing your own `JSON.stringify`.

### 💣 The Variant Codec is different
If you want to emulate `simple-json`'s format you may use the newtype  `TaggedVariant`

```purescript
type YourVariantRow = ( a :: Int, b :: String )
type YourVariant = Variant YourVariantRow
x :: YourVariant
x = inj (Proxy :: Proxy "a") 5
-- encoded = writeJSON x
-- ^ Let's say you had this before
-- You can now do:
encoded = writeJSON (TaggedVariant "type" "value" x)
```
