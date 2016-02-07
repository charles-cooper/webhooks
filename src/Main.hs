{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty as S

import qualified Data.Text.Encoding   as T
import qualified Data.Text.Lazy       as T
import qualified Data.ByteString.Lazy as B

import Control.Monad.IO.Class (liftIO)
import GitHub.WebHook.Handler

scottyHandler :: [String] -> Handler ActionM
scottyHandler keys = Handler keys getBody getHeader where
  getBody = B.toStrict <$> S.body
  getHeader = (fmap . fmap) (T.encodeUtf8 . T.toStrict) . S.header . T.fromStrict . T.decodeUtf8

main :: IO ()
main = scotty 3000 $ do
  matchAny "/" $ do
    res <- runHandler (scottyHandler [])
    case res of
      Left err -> liftIO $ print "derp"
      Right (uuid, payload) -> liftIO $ print payload
