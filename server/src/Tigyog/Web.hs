{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Network.HTTP.Types.Status (status404)

import Tigyog.Git (getCommitMessage, getBlobContents, getMasterFileContents)

import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (fromStrict)

main :: IO ()
main = scotty 3000 $ do
  get "/api/v1/commit/:hash/message" $ do
    hash <- param "hash"
    author <- liftIO $ getCommitMessage hash
    html $ fromStrict author

  -- deprecated.
  -- API should not expose blobs directly.
  -- This makes access control difficult to reason about.
  -- File access is purely by branch+path, below.
  get "/api/v1/blob/:hash" $ do
    hash <- param "hash"
    blobText <- liftIO $ getBlobContents hash
    text $ fromStrict blobText

  --get (regex "^/api/v1/fileoncommit/([^/]*)/(.*)$") $ do
  --  commit <- param "1" -- URL-encoded
  --  path <- param "2"
  --  mBlobText <- liftIO $ getFileContents commit path
  --  case mBlobText of
  --    Just blobText -> do
  --      text $ fromStrict blobText
  --      setHeader "Content-Type" "text/plain; charset=utf-8" -- FIXME scotty should do this https://github.com/scotty-web/scotty/issues/105
  --    Nothing -> status status404

  get (regex "^/api/v1/fileonmaster/(.*)$") $ do
    path <- param "1"
    mBlobText <- liftIO $ getMasterFileContents path
    case mBlobText of
      Just blobText -> do
        text $ fromStrict blobText
        setHeader "Content-Type" "text/plain; charset=utf-8" -- FIXME scotty should do this https://github.com/scotty-web/scotty/issues/105
      Nothing -> status status404
