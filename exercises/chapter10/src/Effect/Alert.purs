module Effect.Alert where

import Prelude

import Effect (kind Effect, Effect)

foreign import data ALERT :: Effect

foreign import alert :: forall eff. String -> Eff (alert :: ALERT | eff) Unit
