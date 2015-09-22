module Data.AddressBook.UI where

import Prelude

import DOM

import Data.Maybe
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.AddressBook
import Data.AddressBook.Validation
import Data.Traversable (sequence)

import Control.Bind

import Control.Monad.Eff
import Control.Monad.Eff.DOM
import Control.Monad.Eff.Console

valueOf :: forall eff. String -> Eff (dom :: DOM | eff) String
valueOf sel = do
  maybeEl <- querySelector sel
  case maybeEl of
    Nothing -> return ""
    Just el -> do
      value <- getValue el
      return $ case read value of
        Right s -> s
        _ -> ""

displayValidationErrors :: forall eff. Array String -> Eff (dom :: DOM | eff) Unit
-- Create a div for each error and add it to the UI.
displayValidationErrors errs = do
  alert <- createElement "div"
    >>= addClass "alert"
    >>= addClass "alert-danger"

  ul <- createElement "ul"
  ul `appendChild` alert

  foreachE errs $ \err -> do
    li <- createElement "li" >>= setText err
    li `appendChild` ul
    return unit

  Just validationErrors <- querySelector "#validationErrors"
  alert `appendChild` validationErrors

  return unit

displayValidationErrorsDiv :: forall eff. Array String -> Eff (dom :: DOM | eff) Unit
displayValidationErrorsDiv errs = do
  alert <- createElement "div"

  foreachE errs $ \err -> do
    div <- createElement "div"
      >>= addClass "alert"
      >>= addClass "alert-danger"
      >>= setText err

    div `appendChild` alert
    return unit

  Just validationErrors <- querySelector "#validationErrors"
  alert `appendChild` validationErrors

  return unit

validateControls :: forall eff. Eff (console :: CONSOLE, dom :: DOM | eff) (Either (Array String) Person)
validateControls = do
  log "Running validators"

  p <- person <$> valueOf "#inputFirstName"
              <*> valueOf "#inputLastName"
              <*> (address <$> valueOf "#inputStreet"
                           <*> valueOf "#inputCity"
                           <*> valueOf "#inputState")
              <*> sequence [ phoneNumber HomePhone <$> valueOf "#inputHomePhone"
                           , phoneNumber CellPhone <$> valueOf "#inputCellPhone"
                           , phoneNumber WorkPhone <$> valueOf "#inputWorkPhone"
                           ]

  return $ validatePerson' p

validateAndUpdateUI :: forall eff. Eff (console :: CONSOLE, dom :: DOM | eff) Unit
validateAndUpdateUI = do
  Just validationErrors <- querySelector "#validationErrors"
  setInnerHTML "" validationErrors

  errorsOrResult <- validateControls

  case errorsOrResult of
    Left errs -> displayValidationErrorsDiv errs
    Right result -> print result

  return unit

setupEventHandlers :: forall eff. Eff (console :: CONSOLE, dom :: DOM | eff) Unit
setupEventHandlers = do
  -- Listen for changes on form fields
  body >>= addEventListener "change" validateAndUpdateUI
  {-- Could have been written as:
  b <- body
  addEventListener "change" validateAndUpdateUI b
  --}

  return unit
