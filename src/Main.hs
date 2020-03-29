{-# LANGUAGE OverloadedStrings #-}

module Main where

import              Network.HTTP.Simple
import qualified    Data.ByteString.Char8           as BS

botURL = "https://api.telegram.org/bot949284451:AAGK8fCgIhv2KLcmT8Mz_bf-3hAl0Ccp7pA/getUpdates"
nnn = "http://httpbin.org/get"


fetchJSON :: IO BS.ByteString
fetchJSON = do
    let request = setRequestProxy (Just (Proxy "127.0.0.1" 8118)) botURL
    res <- httpBS request
    return (getResponseBody res)


main :: IO ()
main = do
  putStrLn "hello world"
  json <- fetchJSON
  BS.putStrLn json

