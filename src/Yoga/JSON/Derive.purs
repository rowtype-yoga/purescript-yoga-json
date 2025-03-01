module Yoga.JSON.Derive where

import Prelude

import Foreign (Foreign, F)
import Safe.Coerce (class Coercible, coerce)
import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

writeVia ∷ ∀ @via a. (Coercible a via) ⇒ WriteForeign via ⇒ a → Foreign
writeVia = writeImpl <<< (coerce ∷ _ → via)

readVia ∷ ∀ @via a. (Coercible a via) ⇒ ReadForeign via ⇒ Foreign → F a
readVia fgn = readImpl fgn # map (coerce ∷ via → _)