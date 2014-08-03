{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty

import Data.Monoid (mconcat)

main = scotty 3000 $ do
  get "/:word" $ do
    word <- param "word"
    html $ mconcat [word]
