{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module User where

import Protolude

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HashMap
import qualified Generics.SOP as SOP

import qualified Language.Elm.Pretty as Pretty
import qualified Language.Elm.Simplification as Simplification
import Language.Haskell.To.Elm

data User a = User
  { name :: Text
  , age :: Int
  , param :: a
  } deriving (Generic, Aeson.ToJSON, SOP.Generic, SOP.HasDatatypeInfo)

instance HasElmType a => HasElmType (User a) where
  elmDefinition =
    Just $ deriveElmTypeDefinition @(User a) defaultOptions "Api.User.User"

instance HasElmDecoder Aeson.Value a => HasElmDecoder Aeson.Value (User a) where
  elmDecoderDefinition =
    Just $ deriveElmJSONDecoder @(User a) defaultOptions Aeson.defaultOptions "Api.User.decoder"

instance HasElmEncoder Aeson.Value a => HasElmEncoder Aeson.Value (User a) where
  elmEncoderDefinition =
    Just $ deriveElmJSONEncoder @(User a) defaultOptions Aeson.defaultOptions "Api.User.encoder"

main :: IO ()
main = do
  let
    definitions =
      Simplification.simplifyDefinition <$>
        jsonDefinitions @(User Int)

    modules =
      Pretty.modules definitions

  forM_ (HashMap.toList modules) $ \(_moduleName, contents) ->
    print contents
