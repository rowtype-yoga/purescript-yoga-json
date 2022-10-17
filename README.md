# purescript-yoga-json

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

`purescript-yoga-json` is almost (read below if you use variants) a drop-in replacement for `purescript-simple-json`. Just change the imports from `Simple.JSON` to `Yoga.JSON`.

## Additions over `simple-json`

### Errors
yoga-json will actually report multiple errors at once in the case of records, objects, and arrays.
There's also a way to make errors more readable with `renderHumanError`:
(e.g. `Must provide a value of type 'Int' instead of 'Object' at $.a.b.c.d[1]`)


### Tuples
There is an inbuilt codec for `Tuple`s thanks to @ursi
`yoga-json` represents tuples as arrays in JSON.

### Eithers
There is an inbuilt codec for `Either`s.
`yoga-json` represents eithers as objects with a `type` and a `value` tag in JSON.

### JSDate and DateTime
Both are encoded as ISO8601 strings as spat out by `JSDate.toISOString`.

### Generics
It includes @justinwoo's codecs for en- and decoding generics inspired by
[simple-json-generics](https://github.com/justinwoo/purescript-simple-json-generics)

It is possible to customise the representation of enums, tagged sum types, and untagged sum types via options.

### BigInts
There is an inbuilt codec for `BigInt`. Writing a `BigInt` however will result in a String. This is a current limitation of JavaScript and the only way around it - it seems - is writing your own JSON.stringify method (which is out of scope of this project). 

## Differences to `simple-json`

### 💣 Variant codec
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
