module Types where

import Prelude

import Effect (Effect)
import Control.Monad.Cont.Trans (ContT)

type Async = ContT Unit Effect
