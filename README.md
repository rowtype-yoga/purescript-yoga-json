# purescript-yoga-json

`yoga-json` is a light-weight and simple json library for purescript. 

**Note**: This library was initially forked from the amazing [simple-json](https://github.com/justinwoo/purescript-simple-json) ([MIT Licence](./LICENSE/simple-json.LICENSE)). See [migrate from `simple-json`](#migrate-from-purescript-simple-json) for migration instructions.

## Table of Contents
* [features](#features)
* [installation](#installation)
* [usage](#usage)

## Features

* 😌 simple and easy to use json codecs
* 🪶 light-weight
* 🤖 built-in support for many common types (Sum types, Tuples, BigInts, Maps, JSDate, DateTime, Eithers, NonEmptyStrings )
* 🖍 human-friendly error reporting 

## Installation

```
spago install yoga-json
```

## Usage

```purescript
import Yoga.JSON as JSON

serialised :: String
serialised =
  JSON.writeJSON { first_name: "Lola", last_name: "Flores" }
```

