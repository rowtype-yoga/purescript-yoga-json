module Yoga.JSON.Error where

import Prelude

import Foreign (ForeignError(..))

toJSONPath ∷ ForeignError → String
toJSONPath fe = "$" <> path
  where
  path = go fe
  go = case _ of
    ForeignError _ → ""
    TypeMismatch _ _ → ""
    ErrorAtIndex i e → "[" <> show i <> "]" <> go e
    ErrorAtProperty _ (TypeMismatch _ "undefined") → ""
    ErrorAtProperty prop e → "." <> prop <> go e

renderHumanError ∷ ForeignError → String
renderHumanError = errorToJSON >>> toHuman
  where
  toHuman { message, path } = message <> " at " <> path

errorToJSON ∷
  ForeignError →
  { message ∷ String
  , path ∷ String
  }
errorToJSON err = go err
  where
  path = toJSONPath err
  go = case _ of
    ForeignError s →
      { path
      , message: s
      }
    TypeMismatch exp "Undefined" →
      { path
      , message: "Must provide a value of type '" <> exp <> "'"
      }
    TypeMismatch exp "undefined" →
      { path
      , message: "Must provide a value of type '" <> exp <> "'"
      }
    TypeMismatch exp act →
      { path
      , message: "Must provide a value of type '" <> exp <> "' instead of '" <> act <> "'"
      }
    ErrorAtIndex _ e → go e
    ErrorAtProperty _ e → go e
