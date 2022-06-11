module Yoga.JSON.Generics.UntaggedSumRep where

import Prelude

import Control.Alt ((<|>))
import Data.Generic.Rep as GR
import Foreign (Foreign)
import Foreign as Foreign
import Yoga.JSON (undefined)
import Yoga.JSON as JSON

readGenericUntaggedSum
  :: forall a rep
   . GR.Generic a rep
  => ReadGenericUntaggedSumRep rep
  => Foreign
  -> Foreign.F a
readGenericUntaggedSum f = GR.to <$> readGenericUntaggedSumRep f

-- | Generic Untagged Sum Representations
-- | Note that because the members are untagged, you should verify your members are not the same type
class ReadGenericUntaggedSumRep rep where
  readGenericUntaggedSumRep :: Foreign -> Foreign.F rep

instance
  ( ReadGenericUntaggedSumRep a
  , ReadGenericUntaggedSumRep b
  ) =>
  ReadGenericUntaggedSumRep (GR.Sum a b) where
  readGenericUntaggedSumRep f = GR.Inl <$> readGenericUntaggedSumRep f
    <|> GR.Inr <$> readGenericUntaggedSumRep f

instance
  ( ReadGenericUntaggedSumRep a
  ) =>
  ReadGenericUntaggedSumRep (GR.Constructor name a) where
  readGenericUntaggedSumRep f = GR.Constructor <$> readGenericUntaggedSumRep f

instance
  ( JSON.ReadForeign a
  ) =>
  ReadGenericUntaggedSumRep (GR.Argument a) where
  readGenericUntaggedSumRep f = GR.Argument <$> JSON.readImpl f

writeGenericUntaggedSum
  :: forall a rep
   . GR.Generic a rep
  => WriteGenericUntaggedSumRep rep
  => a
  -> Foreign
writeGenericUntaggedSum a = GR.from a # writeGenericUntaggedSumRep

-- | Generic Untagged Sum Representations
-- | Note that because the members are untagged, you should verify your members are not the same type
class WriteGenericUntaggedSumRep rep where
  writeGenericUntaggedSumRep :: rep -> Foreign

instance
  ( WriteGenericUntaggedSumRep a
  , WriteGenericUntaggedSumRep b
  ) =>
  WriteGenericUntaggedSumRep (GR.Sum a b) where
  writeGenericUntaggedSumRep = case _ of
    GR.Inl a -> writeGenericUntaggedSumRep a
    GR.Inr a -> writeGenericUntaggedSumRep a

instance
  ( WriteGenericUntaggedSumRep a
  ) =>
  WriteGenericUntaggedSumRep (GR.Constructor name a) where
  writeGenericUntaggedSumRep (GR.Constructor a) = writeGenericUntaggedSumRep a

instance
  ( JSON.WriteForeign a
  ) =>
  WriteGenericUntaggedSumRep (GR.Argument a) where
  writeGenericUntaggedSumRep (GR.Argument a) = JSON.write a