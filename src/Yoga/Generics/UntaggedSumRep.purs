module Yoga.JSON.Generics.UntaggedSumRep where

import Prelude

import Control.Alt ((<|>))
import Data.Generic.Rep as GR
import Foreign (Foreign)
import Foreign as Foreign
import Yoga.JSON as JSON

untaggedSumRep
  :: forall a rep
   . GR.Generic a rep
  => GenericUntaggedSumRep rep
  => Foreign
  -> Foreign.F a
untaggedSumRep f = GR.to <$> genericUntaggedSumRep f

-- | Generic Untagged Sum Representations
-- | Note that because the members are untagged, you should verify your members are not the same type
class GenericUntaggedSumRep rep where
  genericUntaggedSumRep :: Foreign -> Foreign.F rep

instance
  ( GenericUntaggedSumRep a
  , GenericUntaggedSumRep b
  ) =>
  GenericUntaggedSumRep (GR.Sum a b) where
  genericUntaggedSumRep f = GR.Inl <$> genericUntaggedSumRep f
    <|> GR.Inr <$> genericUntaggedSumRep f

instance
  ( GenericUntaggedSumRep a
  ) =>
  GenericUntaggedSumRep (GR.Constructor name a) where
  genericUntaggedSumRep f = GR.Constructor <$> genericUntaggedSumRep f

instance
  ( JSON.ReadForeign a
  ) =>
  GenericUntaggedSumRep (GR.Argument a) where
  genericUntaggedSumRep f = GR.Argument <$> JSON.readImpl f
