module Main where

import Prelude

import Control.Monad.Except (runExcept)
import Data.AddressBook (Address(..), Person(..), PhoneNumber(..), examplePerson)
import Data.AddressBook.Validation (Errors, validatePerson')
import Data.Array ((..), length, modifyAt, zipWith)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (fromJust, fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import Foreign (ForeignError, readString, unsafeToForeign)
import Foreign.Index (index)
import Partial.Unsafe (unsafePartial)
import React (ReactClass, ReactThis)
import React as React
import React.DOM as D
import React.DOM.Props as P
import React.SyntheticEvent (SyntheticInputEvent)
import ReactDOM as ReactDOM
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

type AppState =
  { person :: Person
  , errors :: Errors
  }

initialState :: AppState
initialState =
  { person: examplePerson
  , errors: []
  }

valueOf :: SyntheticInputEvent -> Either (NonEmptyList ForeignError) String
valueOf e = runExcept do
  target <- index (unsafeToForeign e) "target"
  value <- index target "value"
  readString value

updateAppState
  :: forall props
   . ReactThis props AppState
  -> (String -> Person)
  -> SyntheticInputEvent
  -> Effect Unit
updateAppState this update e =
  for_ (valueOf e) \s -> do
    let newPerson = update s
    log "Running validators"
    case validatePerson' newPerson of
      Left errors -> React.writeState this { person: newPerson, errors }
      Right _ -> React.writeState this { person: newPerson, errors: [] }

addressBook :: ReactClass { }
addressBook = React.component "AddressBook" component
  where
  component this =
    pure { state: initialState
         , render: render <$> React.getState this
         }
    where
    render { person: Person person@{ homeAddress: Address address }, errors } =
      let renderValidationError err = D.li' [ D.text err ]
          renderValidationErrors [] = []
          renderValidationErrors xs =
            [ D.div
              [ P.className "alert alert-danger" ]
              [ D.ul' (map renderValidationError xs) ]
            ]

          formField name hint value update =
            D.div
            [ P.className "form-group" ]
            [ D.label
              [ P.className "col-sm-2 control-label" ]
              [ D.text name ]
            , D.div
              [ P.className "col-sm-3" ]
              [ D.input
                [ P._type "text"
                , P.className "form-control"
                , P.placeholder hint
                , P.value value
                , P.onChange (updateAppState this update)
                ]
              ]
            ]

          renderPhoneNumber (PhoneNumber phone) index =
            formField (show phone."type") "XXX-XXX-XXXX" phone.number \s ->
              Person $ person { phones = fromMaybe person.phones $ modifyAt index (updatePhoneNumber s) person.phones }

          updateFirstName s = Person $ person { firstName = s }
          updateLastName  s = Person $ person { lastName  = s }

          updateStreet s = Person $ person { homeAddress = Address $ address { street = s } }
          updateCity   s = Person $ person { homeAddress = Address $ address { city   = s } }
          updateState  s = Person $ person { homeAddress = Address $ address { state  = s } }

          updatePhoneNumber s (PhoneNumber o) = PhoneNumber $ o { number = s }
      in
        D.div
        [ P.className "container" ]
        [ D.div
          [ P.className "row" ]
          (renderValidationErrors errors)
        , D.div
          [ P.className "row" ]
          [ D.form
            [ P.className "form-horizontal" ] $
            [ D.h3'[ D.text "Basic Information" ]
            , formField "First Name" "First Name" person.firstName updateFirstName
            , formField "Last Name"  "Last Name"  person.lastName  updateLastName
            , D.h3' [ D.text "Address" ]
            , formField "Street" "Street" address.street updateStreet
            , formField "City"   "City"   address.city   updateCity
            , formField "State"  "State"  address.state  updateState
            , D.h3' [ D.text "Contact Information" ]
            ] <> zipWith renderPhoneNumber person.phones (0 .. length person.phones)
          ]
        ]

main :: Effect Unit
main = void do
  log "Rendering address book component"
  doc <- window >>= document
  let node = toNonElementParentNode doc
  element <- getElementById "main" node
  let element' = unsafePartial (fromJust element)
  ReactDOM.render (React.createLeafElement addressBook { }) element'