{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- | A dropbox client
--
module Dropbox
  ( createClient
  , cliRoutes
  , Dropbox(..)
  , defListFolderRequest
  , FileTag(..)
  , Entry(..)
  , ListFolderResponse(..)
  , ListFolderRequest
  , path
  , LinkResponse(..)
  , LinkRequest(..)
  , TokenRequest(..)
  , TokenBody(..)
  , dropboxProxy
  ) where

import Control.Exception(throwIO)
import Data.Proxy
import Data.Text(Text)
import Network.HTTP.Client.TLS
import Servant.API
import Servant.Client.Generic
import Servant.API.Generic
import Data.Aeson
import Servant.Auth
import Servant.Auth.Client
import Servant.Client
import Web.FormUrlEncoded hiding (fieldLabelModifier)
import Data.Char (isLower)

toSnake :: String -> String
toSnake = camelTo2 '_' . dropWhile isLower

data ListFolderRequest = ListFolderRequest
  { path :: String
  , recursive :: Bool
  , include_media_info :: Bool
  , include_deleted :: Bool
  , include_has_explicit_shared_members :: Bool
  , include_mounted_folders :: Bool
  , include_non_downloadable_files :: Bool
} deriving (Generic, ToJSON)

newtype LinkRequest = LinkRequest
  { linkPath :: String
  } deriving (Generic)

instance ToJSON LinkRequest where
    -- this generates a Value
    toJSON (LinkRequest lpath) =
        object ["path" .= lpath]

defListFolderRequest :: ListFolderRequest
defListFolderRequest = ListFolderRequest
  { path = "" -- root is empty string in dropbox api
  , recursive = False
  , include_media_info = False
  , include_deleted = False
  , include_has_explicit_shared_members = False
  , include_mounted_folders = True
  , include_non_downloadable_files = False
  }

data FileTag = File | Folder
  deriving (Generic, FromJSON, Show, Eq)

data Entry = Entry
  { eTag :: FileTag
  , eName :: Text
  , ePathDisplay :: Text
  , eId :: Text
  } deriving (Generic, Show, Eq)

instance FromJSON Entry where
  parseJSON = withObject "Entry" $ \v -> do
    tag' :: Text <- v .: ".tag"
    let eTag = if tag' == "file" then File else Folder
    eName <- v .: "name"
    ePathDisplay <- v .: "path_display"
    eId <- v .: "id"
    pure $ Entry {..}

newtype ListFolderResponse = ListFolderResponse
  { entries :: [Entry]
  } deriving (Generic, FromJSON, Show, Eq)

data TokenRequest = TokenRequest
  { trAccessToken :: Text
  , trExpiresIn :: Maybe Int -- seconds
  , trTokenType :: Text
  , trRefreshToken :: Maybe Text
  , trScope :: Text
  , trAccountId :: Text
  , trUid :: Text
  } deriving Generic

snakeConstructor :: Options
snakeConstructor = defaultOptions { constructorTagModifier = camelTo2 '_' }

snakeLabel :: Options
snakeLabel = snakeConstructor { fieldLabelModifier = toSnake }

instance FromJSON TokenRequest where
  parseJSON = genericParseJSON snakeLabel

data TokenBody = TokenBody
  { code         :: Maybe Text
  , refresh_token :: Maybe Text
  , grant_type    :: Text
  , redirect_uri  :: Text
  , client_id     :: Text
  , client_secret :: Text
  } deriving (Generic, Show, ToForm)

newtype LinkResponse = LinkResponse {
  link :: Text
  } deriving (Generic, Show, FromJSON)

-- | To use the various endpoints make sure you have the right
-- scope in your 'app' on dropbox: https://www.dropbox.com/developers/apps/info/t282kls5wbrtofs#permissions
-- then regenerate your token (because it's attached to that)
data Dropbox route = Dropbox
  -- TODO: implement list folder
  {
  -- | https://www.dropbox.com/developers/documentation/http/documentation#files-list_folder
    _dropbox_list_folder :: route :- "2" :> "files" :> "list_folder" :> Auth '[Bearer] Token :> ReqBody '[JSON] ListFolderRequest :> Post '[JSON] ListFolderResponse
  -- | https://www.dropbox.com/developers/documentation/http/documentation#files-get_temporary_link
  , _dropbox_get_temporary_link :: route :- "2" :> "files" :> "get_temporary_link" :> Auth '[Bearer] Token :> ReqBody '[JSON] LinkRequest :> Post '[JSON] LinkResponse
  -- | https://www.dropbox.com/developers/documentation/http/documentation#oauth2-token
  , _dropbox_token :: route :- "oauth2" :> "token" :> ReqBody '[FormUrlEncoded] TokenBody :> Post '[JSON] TokenRequest
  } deriving Generic

-- does the generic to type level compute
dropboxProxy :: Proxy (ToServant Dropbox AsApi)
dropboxProxy = genericApi (Proxy @Dropbox)

-- | gives an adhoc client. This throws exceptions
cliRoutes ::  ClientEnv -> Dropbox (AsClientT IO)
cliRoutes env = genericClientHoist
    (\x -> runClientM x env >>= either throwIO return)

createClient :: IO (Dropbox (AsClientT IO))
createClient = do
  baseUri <- parseBaseUrl "https://api.dropboxapi.com/"
  manager <- newTlsManager
  pure $ cliRoutes $ mkClientEnv manager baseUri
