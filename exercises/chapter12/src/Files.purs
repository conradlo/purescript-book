module Files where

import Prelude

import Control.Monad.Cont.Trans (ContT(..))
import Effect (Effect)
import Control.Monad.Except.Trans (ExceptT(..))
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn4, Fn3, runFn4, runFn3)
import Types (Async)

type ErrorCode = String

type FilePath = String

foreign import readFileImpl ::
                 Fn3 FilePath
                   (String -> Effect Unit)
                   (ErrorCode -> Effect Unit)
                   (Effect Unit)

foreign import writeFileImpl ::
                 Fn4 FilePath
                   String
                   (Effect Unit)
                   (ErrorCode -> Effect Unit)
                   (Effect Unit)

readFile :: FilePath -> (Either ErrorCode String -> Effect Unit) -> Effect Unit
readFile path k = runFn3 readFileImpl path (k <<< Right) (k <<< Left)

writeFile :: FilePath -> String -> (Either ErrorCode Unit -> Effect Unit) -> Effect Unit
writeFile path text k = runFn4 writeFileImpl path text (k $ Right unit) (k <<< Left)

readFileCont :: FilePath -> Async (Either ErrorCode String)
readFileCont path = ContT $ readFile path

writeFileCont :: FilePath -> String -> Async (Either ErrorCode Unit)
writeFileCont path text = ContT $ writeFile path text

readFileContEx :: FilePath -> ExceptT ErrorCode Async String
readFileContEx path = ExceptT $ readFileCont path

writeFileContEx :: FilePath -> String -> ExceptT ErrorCode Async Unit
writeFileContEx path text = ExceptT $ writeFileCont path text

copyFileContEx :: FilePath -> FilePath -> ExceptT ErrorCode Async Unit
copyFileContEx src dest = do
  content <- readFileContEx src
  writeFileContEx dest content