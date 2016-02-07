{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Web.Scotty as S

import qualified Data.Text.Encoding   as T
import qualified Data.Text.Lazy       as T
import qualified Data.ByteString.Lazy as B

import Data.Monoid

import Control.Monad.IO.Class (liftIO)
import GitHub.WebHook.Handler as Git

deriving instance Show Git.Error

scottyHandler :: [String] -> Handler ActionM
scottyHandler keys = Handler keys getBody getHeader where
  getBody = do
    ret <- S.body
    liftIO $ putStr $ "debug body: "
    liftIO $ B.putStrLn ret
    return $ B.toStrict ret
  getHeader = (fmap . fmap) (T.encodeUtf8 . T.toStrict) . S.header . T.fromStrict . T.decodeUtf8

main :: IO ()
main = scotty 3000 $ do
  matchAny "/" $ do
    res <- runHandler (scottyHandler [])
    case res of
      Left err -> liftIO $ print err
      Right (uuid, payload) -> liftIO $ print payload
