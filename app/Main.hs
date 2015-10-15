{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import App (serve)

import BasePrelude

main :: IO ()
main = serve 6666
