{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Helper functions for parsing values from @JSString@s

module Cardano.Prelude.Json.Parse
  ( parseJSString
  )
where

import Cardano.Prelude.Base

import Data.String (String)
import Data.Typeable (typeRep)
import Formatting (Format, build, formatToString, string)
import Formatting.Buildable (Buildable)
import Text.JSON.Canonical
  ( JSValue(JSString)
  , ReportSchemaErrors(expected)
  , expectedButGotValue
  , fromJSString
  )


-- | Attempt to parse a value of type @a@ from the body of a @JSString@ using
--   @parser@
parseJSString
  :: forall a m e
   . (Typeable a, ReportSchemaErrors m, Buildable e)
  => (Text -> Either e a)
  -> JSValue
  -> m a
parseJSString parser = \case
  JSString str ->
    either (report $ fromJSString str) pure . parser . toS $ fromJSString str
  val -> expectedButGotValue typeName val
 where
  typeName :: String
  typeName = show $ typeRep (Proxy @a)

  report :: String -> e -> m a
  report str err =
    expected typeName (Just $ formatToString errFormat str err)

  errFormat :: Format r (String -> e -> r)
  errFormat =
    "Failed to parse value from JSString "
      . string
      . "\n"
      . "Parser failed with error: "
      . build
