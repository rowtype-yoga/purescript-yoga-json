# purescript-yoga-json

`yoga-json` is a light-weight and simple json library for purescript. 

**Note**: This library was initially forked from the amazing [simple-json](https://github.com/justinwoo/purescript-simple-json) ([MIT Licence](./LICENSE/simple-json.LICENSE)).
Replace your imports from `Simple.JSON` to `Yoga.JSON` to migrate.
## Table of Contents
* [features](#features)
* [installation](#installation)
* [usage](#usage)

## Features

* 😌 simple and easy to use json codecs
* 🪶 light-weight
* 🤖 built-in support for many common types (`Sum` types, Tuples, BigInts, Maps, JSDate, DateTime, Eithers, NonEmptyStrings )
* 🖍 human-friendly error reporting 

## Installation

```
spago install yoga-json
```

## Usage

`purescript-yoga-json` basically provides two functions `writeJSON` and `readJSON`.

Use `writeJSON` to serialise a type to a JSON string:

```purescript
import Yoga.JSON as JSON

serialised :: String
serialised =
  JSON.writeJSON { first_name: "Lola", last_name: "Flores" }
  -- {"last_name":"Flores","first_name":"Lola"}
```

Use `readJSON` to deserialise a JSON string:
```purescript
import Yoga.JSON as JSON

deserialised :: Either MultipleErrors { first_name :: String, last_name :: String } 
deserialised = JSON.readJSON """{ "first_name": "Lola", "last_name": "Flores" }"""
-- Right { first_name: "Lola", last_name: "Flores" }
```

As the parsing can fail, `readJSON` returns an `Either`, potentially containing a `Left` data constructor with `MultipleErrors`. If you don't care about the specific errors, you can use `readJSON_`, which returns a `Maybe`:

```purescript
deserialised :: Maybe { first_name :: String, last_name :: String } 
deserialised = JSON.readJSON_ """{ "first_name": "Lola", "last_name": "Flores" }"""
-- Just { first_name: "Lola", last_name: "Flores" }
```


## Sum types

`purescript-yoga-json` provides utility functions to easily generate serialisers and deserialisers for your sum types.

Let's start with a simple example of a sum type where our data constructors do not contain any further values:

```
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Yoga.JSON as JSON
import Yoga.JSON.Generics (genericReadForeignEnum, genericWriteForeignEnum)
import Yoga.JSON.Generics.EnumSumRep as Enum

data MyEnum = Enum1 | Enum2 | Enum3
```

Now, we need to derive a `Generic` instance for it:

```
derive instance Generic MyEnum _
-- and optionally a Show instance
instance Show MyEnum where
  show = genericShow
```

Next, we define a `WriteForeign` instance and implement the `writeImpl` function using `genericWriteForeignEnum Enum.defaultOptions`. 

```purescript
instance WriteForeign MyEnum
  where
  writeImpl = genericWriteForeignEnum Enum.defaultOptions
```

Similarly, we implement the `ReadForeign` instance using `genericReadForeignEnum Enum.defaultOptions`
```purescript
instance ReadForeign MyEnum
  where
  readImpl = genericReadForeignEnum Enum.defaultOptions
```

That's all, we can now serialise our data type:
```purescript
serialised = writeJSON { "myEnum": Enum3 }
-- {"myEnum":"Enum3"}
```
and deserialise it:
```purescript
deserialised :: E { "myEnum" :: MyEnum }
deserialised = readJSON serialised
-- Right { myEnum: Enum3 }
```

### Writing custom codecs

In order to write your own, custom codecs you will need to provide instances for `WriteForeign` and `ReadForeign`. 

Let's see an example. We define a simple data type `TrafficLight` that has three data constructors `Red`, `Yellow` and `Green`:
```purescript
data TrafficLight = Red | Yellow | Green
```
We would like to serialise these constructors as lower case strings `"red"`, `"yellow"` and `"green"`.

First we need to provide an implementation for the `WriteForeign` type class, that tells `yoga-json` how to serialise the type. We pattern match on the three different data constructors and write them as the three `String`s red, yellow and green, using the same `writeImpl` function:
```purescript
instance WriteForeign TrafficLight where
  writeImpl Red = writeImpl "red"
  writeImpl Yellow = writeImpl "yellow"
  writeImpl Green = writeImpl "green"
```
This works, because `yoga-json` already knows how to serialise a `String`. 

Similarly, we need to provide an implementation for the `ReadForeign` type class, that tells `yoga-json` how to deserialise the type. We start by deserialising into a primitive type, typically a String or an Object, and then convert it to our desired Purescript type.

Since we don't know the input `String`, it might contain invalid data and therefore deserialisation might fail. `yoga-json` uses the `ExceptT` monad to reflect this. We can return valid values using `pure` and `fail` on invalid values:

```purescript
instance ReadForeign TrafficLight where
  readImpl json = do
    -- deserialise into a string
    str :: String <- readImpl json  
    -- now we pattern match on our valid types
    case str of
      "red" -> pure Red
      "yellow" -> pure Yellow
      "green" -> pure Green
      -- and fail if we get an invalid type
      other -> fail $ ForeignError $ "Failed to parse " <> other <> " as TrafficLight"
```

Now we can serialise our data type:
```purescript
serialised = writeJSON { "trafficLight": Red }
-- {"trafficLight":"red"}
```

and deserialise it:
```purescript
deserialised :: E { "trafficLight" :: TrafficLight }
deserialised = readJSON """{ "trafficLight": "green" }"""
-- Right { trafficLight: Green }

deserialisedUnknown :: E { "trafficLight" :: TrafficLight }
deserialisedUnknown = readJSON """{ "trafficLight": "purple" }"""
-- Left (NonEmptyList (NonEmpty (ErrorAtProperty "trafficLight" (ForeignError "Failed to parse purple as TrafficLight")) Nil))
```
