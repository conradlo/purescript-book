module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Data.AddressBook (examplePerson)
import Data.AddressBook.Validation (validatePerson)

main :: Effect Unit
main = logShow (validatePerson examplePerson)
