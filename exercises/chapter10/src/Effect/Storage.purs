module Effect.Storage where

import Prelude

import Effect (kind Effect, Effect)
import Foreign

foreign import data STORAGE :: Effect

foreign import setItem :: forall eff. String -> String -> Eff (storage :: STORAGE | eff) Unit

foreign import getItem :: forall eff. String -> Eff (storage :: STORAGE | eff) Foreign
