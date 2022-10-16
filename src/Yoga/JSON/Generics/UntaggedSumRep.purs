module Yoga.JSON.Generics.UntaggedSumRep where

import Prelude

import Control.Alt ((<|>))
import Data.Generic.Rep as GR
import Foreign (Foreign)
import Foreign as Foreign
import Yoga.JSON as JSON

genericReadForeignUntaggedSum ∷
  ∀ a rep.
  GR.Generic a rep ⇒
  ReadGenericUntaggedSumRep rep ⇒
  Foreign →
  Foreign.F a
genericReadForeignUntaggedSum f = GR.to <$> genericReadForeignUntaggedSumRep f

-- | Generic Untagged Sum Representations
-- | Note that because the members are untagged, you should verify your members are not the same type
class ReadGenericUntaggedSumRep rep where
  genericReadForeignUntaggedSumRep ∷ Foreign → Foreign.F rep

instance
  ( ReadGenericUntaggedSumRep a
  , ReadGenericUntaggedSumRep b
  ) ⇒
  ReadGenericUntaggedSumRep (GR.Sum a b) where
  genericReadForeignUntaggedSumRep f = GR.Inl <$> genericReadForeignUntaggedSumRep f
    <|> GR.Inr <$> genericReadForeignUntaggedSumRep f

instance
  ( ReadGenericUntaggedSumRep a
  ) ⇒
  ReadGenericUntaggedSumRep (GR.Constructor name a) where
  genericReadForeignUntaggedSumRep f = GR.Constructor <$> genericReadForeignUntaggedSumRep f

instance
  ( JSON.ReadForeign a
  ) ⇒
  ReadGenericUntaggedSumRep (GR.Argument a) where
  genericReadForeignUntaggedSumRep f = GR.Argument <$> JSON.readImpl f

genericWriteForeignUntaggedSum ∷
  ∀ a rep.
  GR.Generic a rep ⇒
  WriteGenericUntaggedSumRep rep ⇒
  a →
  Foreign
genericWriteForeignUntaggedSum a = GR.from a # genericWriteForeignUntaggedSumRep

-- | Generic Untagged Sum Representations
-- | Note that because the members are untagged, you should verify your members are not the same type
class WriteGenericUntaggedSumRep rep where
  genericWriteForeignUntaggedSumRep ∷ rep → Foreign

instance
  ( WriteGenericUntaggedSumRep a
  , WriteGenericUntaggedSumRep b
  ) ⇒
  WriteGenericUntaggedSumRep (GR.Sum a b) where
  genericWriteForeignUntaggedSumRep = case _ of
    GR.Inl a → genericWriteForeignUntaggedSumRep a
    GR.Inr a → genericWriteForeignUntaggedSumRep a

instance
  ( WriteGenericUntaggedSumRep a
  ) ⇒
  WriteGenericUntaggedSumRep (GR.Constructor name a) where
  genericWriteForeignUntaggedSumRep (GR.Constructor a) = genericWriteForeignUntaggedSumRep a

instance
  ( JSON.WriteForeign a
  ) ⇒
  WriteGenericUntaggedSumRep (GR.Argument a) where
  genericWriteForeignUntaggedSumRep (GR.Argument a) = JSON.write a