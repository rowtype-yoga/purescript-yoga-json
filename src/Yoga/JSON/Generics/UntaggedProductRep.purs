module Yoga.JSON.Generics.UntaggedProductRep where

import Prelude

import Data.Array as Array
import Data.Generic.Rep as GR
import Foreign (Foreign)
import Foreign as Foreign
import Foreign.Index (readIndex)
import Yoga.JSON as JSON

genericReadForeignUntaggedProduct ∷
  ∀ a rep.
  GR.Generic a rep ⇒
  ReadGenericUntaggedProduct rep ⇒
  Foreign →
  Foreign.F a
genericReadForeignUntaggedProduct f = GR.to <$> genericReadForeignUntaggedProductRep 0 f

type Offset = Int

-- | Generic Untagged Product Representations, as a heterogeneous fixed-length array
-- | You should consider using records instead in almost any usage.
class ReadGenericUntaggedProduct rep where
  genericReadForeignUntaggedProductRep ∷ Offset → Foreign → Foreign.F rep

instance
  ( ReadGenericUntaggedProduct a
  ) ⇒
  ReadGenericUntaggedProduct (GR.Constructor name a) where
  genericReadForeignUntaggedProductRep i f = GR.Constructor <$> genericReadForeignUntaggedProductRep i
    f

instance
  ( JSON.ReadForeign a
  ) ⇒
  ReadGenericUntaggedProduct (GR.Argument a) where
  genericReadForeignUntaggedProductRep i f = do
    x ← JSON.readImpl =<< readIndex i f
    pure (GR.Argument x)

instance
  ( ReadGenericUntaggedProduct a
  , ReadGenericUntaggedProduct b
  ) ⇒
  ReadGenericUntaggedProduct (GR.Product a b) where
  genericReadForeignUntaggedProductRep i f =
    GR.Product
      <$> genericReadForeignUntaggedProductRep i f
      <*> genericReadForeignUntaggedProductRep (i + 1) f

genericWriteForeignUntaggedProduct ∷
  ∀ a rep.
  GR.Generic a rep ⇒
  WriteGenericUntaggedProduct rep ⇒
  a →
  Foreign
genericWriteForeignUntaggedProduct a =
  JSON.write (genericWriteForeignUntaggedProductRep [] (GR.from a))

-- | Generic Untagged Product Representations, as a heterogeneous fixed-length array
-- | You should consider using records instead in almost any usage.
class WriteGenericUntaggedProduct rep where
  genericWriteForeignUntaggedProductRep ∷ Array Foreign → rep → Array Foreign

instance
  ( WriteGenericUntaggedProduct a
  ) ⇒
  WriteGenericUntaggedProduct (GR.Constructor name a) where
  genericWriteForeignUntaggedProductRep arr (GR.Constructor a) =
    genericWriteForeignUntaggedProductRep arr a

instance
  ( JSON.WriteForeign a
  ) ⇒
  WriteGenericUntaggedProduct (GR.Argument a) where
  genericWriteForeignUntaggedProductRep arr (GR.Argument a) =
    Array.snoc arr (JSON.write a)

instance
  ( WriteGenericUntaggedProduct a
  , WriteGenericUntaggedProduct b
  ) ⇒
  WriteGenericUntaggedProduct (GR.Product a b) where
  genericWriteForeignUntaggedProductRep arr (GR.Product a b) =
    genericWriteForeignUntaggedProductRep (genericWriteForeignUntaggedProductRep arr a) b
