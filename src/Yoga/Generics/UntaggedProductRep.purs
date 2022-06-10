module Yoga.JSON.Generics.UntaggedProductRep where

import Prelude

import Data.Generic.Rep as GR
import Foreign (Foreign)
import Foreign as Foreign
import Foreign.Index (readIndex)
import Yoga.JSON as JSON

untaggedProductRep
  :: forall a rep
   . GR.Generic a rep
  => GenericUntaggedProductRep rep
  => Foreign
  -> Foreign.F a
untaggedProductRep f = GR.to <$> genericUntaggedProductRep 0 f

type Offset = Int

-- | Generic Untagged Product Representations, as a heterogeneous fixed-length array
-- | You should consider using records instead in almost any usage.
class GenericUntaggedProductRep rep where
  genericUntaggedProductRep :: Offset -> Foreign -> Foreign.F rep

instance untaggedProductRepConstructor ::
  ( GenericUntaggedProductRep a
  ) =>
  GenericUntaggedProductRep (GR.Constructor name a) where
  genericUntaggedProductRep i f = GR.Constructor <$> genericUntaggedProductRep i
    f

instance untaggedProductRepArgument ::
  ( JSON.ReadForeign a
  ) =>
  GenericUntaggedProductRep (GR.Argument a) where
  genericUntaggedProductRep i f = do
    x <- JSON.readImpl =<< readIndex i f
    pure (GR.Argument x)

instance untaggedProductRepProduct ::
  ( GenericUntaggedProductRep a
  , GenericUntaggedProductRep b
  ) =>
  GenericUntaggedProductRep (GR.Product a b) where
  genericUntaggedProductRep i f =
    GR.Product
      <$> genericUntaggedProductRep i f
      <*> genericUntaggedProductRep (i + 1) f
