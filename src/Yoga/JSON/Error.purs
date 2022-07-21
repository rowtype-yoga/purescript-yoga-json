module Yoga.JSON.Error where

import Prelude

import Foreign (ForeignError(..))

toJSONPath :: ForeignError -> String
toJSONPath fe = "$" <> path
  where
  path = go fe
  go = case _ of
    ForeignError _ -> ""
    TypeMismatch _ _ -> ""
    ErrorAtIndex i e -> "[" <> show i <> "]" <> go e
    ErrorAtProperty _ (TypeMismatch _ "undefined") -> ""
    ErrorAtProperty prop e -> "." <> prop <> go e
