{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Web.Scotty as S
import Data.Aeson
import Data.Aeson.TH

import qualified Data.Text.Lazy       as T
import qualified Data.ByteString.Lazy as B

import Data.Monoid

import Control.Monad.IO.Class (liftIO)

import GHC.Generics

-------------------------
-- Github webhook headers
-------------------------

githubEvent    :: T.Text
githubEvent    = "X-Github-Event"

hubSignature   :: T.Text
hubSignature   = "X-Hub-Signature"

githubDelivery :: T.Text
githubDelivery = "X-Github-Delivery"
 
--------------------------
-- Github webhook payloads
--------------------------

data WebhookEvent
  = Ping PingEvent
  | Push PushEvent
  deriving (Generic, Show)

data PingEvent = PingEvent
  { _pingzen     :: T.Text
  , _pinghook_id :: Integer
  -- unimplemented: ping hook
  } deriving (Generic, Show)

type GitRef = T.Text -- GitRef { unGitRef :: T.Text } deriving Show

data PushEvent = PushEvent
  { _pushref     :: T.Text
  , _pushbefore  :: GitRef
  , _pushafter   :: GitRef
  , _pushcreated :: Bool
  , _pushdeleted :: Bool
  , _pushforced  :: Bool
  -- unimplemented fields
  } deriving (Generic, Show)

instance FromJSON PushEvent where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 5 }

main :: IO ()
main = scotty 3000 $ do
  post "/" $ do
    evtTy <- header githubEvent
    res <- case evtTy of
      Nothing     -> raise $ "Header '" <> githubEvent <> "' not found"
      Just "push" -> Right . Push <$> jsonData
      Just e      -> pure . Left $ "Unimplemented webhook event: " <> e
    liftIO $ print res

