module Network.HTTP.Client where

import Prelude

import Control.Monad.Cont.Trans (ContT(..))
import Effect (Effect)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)
import Types (Async)

type URI = String

foreign import getImpl ::
                 Fn3 URI
                   (String -> Effect Unit)
                   (String -> Effect Unit)
                   (Effect Unit)

get :: URI -> Async  (Either String String)
get req = ContT $ \k -> runFn3 getImpl req (k <<< Right) (k <<< Left)