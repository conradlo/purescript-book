module Data.AddressBook.Validation where

import Prelude

import Data.AddressBook (Address(..), Person(..), PhoneNumber(..), PhoneType(..), address, person, phoneNumber)
import Data.Either (Either(..))
import Data.String (length)
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, unV, invalid)
import Partial.Unsafe (unsafePartial)

data Field = FirstNameField
            | LastNameField
            | StreetField
            | CityField
            | StateField
            | PhoneField PhoneType

instance showField :: Show Field where
  show FirstNameField = "FirstNameField"
  show LastNameField = "LastNameField"
  show StreetField = "StreetField"
  show CityField = "CityField"
  show StateField = "StateField"
  show (PhoneField pType) = "PhoneField" <> show pType

instance eqField :: Eq Field where
 eq l r = (show l) == (show r)

data ValidationError = ValidationError String Field

type Errors = Array ValidationError

fieldDisplayName :: Field -> String
fieldDisplayName field = case field of 
  FirstNameField -> "First Name"
  LastNameField -> "Last Name"
  StreetField -> "Street"
  CityField -> "City"
  StateField -> "State"
  PhoneField pType -> case pType of 
    HomePhone -> "Home Phone Number"
    WorkPhone -> "Work Phone Number"
    CellPhone -> "Cell Phone Number"
    OtherPhone -> "Other Phone Number"

getErrorMessage :: ValidationError -> String
getErrorMessage (ValidationError str _) = str

nonEmpty :: Field -> String -> V Errors Unit
nonEmpty field "" = invalid [ValidationError ("Field '" <> fieldDisplayName field <> "' cannot be empty") field]
nonEmpty _     _  = pure unit

arrayNonEmpty :: forall a. Field -> Array a -> V Errors Unit
arrayNonEmpty field [] = invalid [ValidationError ("Field '" <> fieldDisplayName field <> "' must contain at least one value") field]
arrayNonEmpty _     _  = pure unit

lengthIs :: Field -> Int -> String -> V Errors Unit
lengthIs field len value | length value /= len = invalid [ValidationError ("Field '" <> fieldDisplayName field <> "' must have length " <> show len) field]
lengthIs _     _   _     = pure unit

phoneNumberRegex :: Regex
phoneNumberRegex =
  unsafePartial
    case regex "^\\d{3}-\\d{3}-\\d{4}$" noFlags of
      Right r -> r

matches :: Field -> Regex -> String -> V Errors Unit
matches _     regex value | test regex value = pure unit
matches field _     _     = invalid [ValidationError ("Field '" <> fieldDisplayName field <> "' did not match the required format") field]

validateAddress :: Address -> V Errors Address
validateAddress (Address o) =
  address <$> (nonEmpty StreetField o.street *> pure o.street)
          <*> (nonEmpty CityField   o.city   *> pure o.city)
          <*> (lengthIs StateField 2 o.state *> pure o.state)

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber (PhoneNumber o) =
  phoneNumber <$> pure o."type"
              <*> (matches (PhoneField o."type") phoneNumberRegex o.number *> pure o.number)

validatePerson :: Person -> V Errors Person
validatePerson (Person o) =
  person <$> (nonEmpty FirstNameField o.firstName *> pure o.firstName)
         <*> (nonEmpty LastNameField  o.lastName  *> pure o.lastName)
         <*> validateAddress o.homeAddress
         <*> (traverse validatePhoneNumber o.phones)

validatePerson' :: Person -> Either Errors Person
validatePerson' p = unV Left Right $ validatePerson p
