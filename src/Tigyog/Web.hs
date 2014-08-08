{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty

import Tigyog.Git (getCommitMessage)

import Data.Monoid (mconcat)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (fromStrict)

main = scotty 3000 $ do
  get "/api/v1/commit/:hash/message" $ do
    hash <- param "hash"
    author <- liftIO $ getCommitMessage hash
    html $ fromStrict author
