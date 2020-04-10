{-# LANGUAGE OverloadedStrings #-}

module Main where

import              Network.HTTP.Simple
import qualified    Data.ByteString.Lazy.Char8      as L8

botURL = "https://api.telegram.org/bot949284451:AAGK8fCgIhv2KLcmT8Mz_bf-3hAl0Ccp7pA/getUpdates"
nnn = "http://httpbin.org/get"

{-
working proxy
"45.151.169.28" 80
-}


fetchJSON :: IO L8.ByteString
fetchJSON = do
    let request = setRequestProxy (Just (Proxy "45.151.169.28" 80)) botURL
    res <- httpLBS request
    return (getResponseBody res)


main :: IO ()
main = do
  putStrLn "hello world"
  json <- fetchJSON
  L8.putStrLn json

