{-# LANGUAGE OverloadedStrings #-}

module Main where

import            Network.HTTP.Simple
import qualified  Data.ByteString.Lazy    as B
import            Data.Aeson
import            Data.Aeson.Types        (parseMaybe)
import            Control.Monad           (mzero)
import            Control.Applicative     ((<$>), (<*>))

-- single message
data Message = Message  { senderName :: String, text :: String }
instance FromJSON Message where
  parseJSON (Object msg)  = Message
                            <$>
                            -- extracting sender of a message (message -> from -> username)
                            (msg .: "message" >>= (.: "from") >>= (.: "username"))
                            <*>
                            -- extracting text of a message (message -> text)
                            (msg .: "message" >>= (.: "text"))
  parseJSON _             = mzero

-- list of messages
data Messages = Messages [Message]
instance FromJSON Messages where
  parseJSON (Object msgs) = Messages <$> msgs .: "result"

botURL = "https://api.telegram.org/bot949284451:AAGK8fCgIhv2KLcmT8Mz_bf-3hAl0Ccp7pA/getUpdates"

fetchJSON :: IO B.ByteString
fetchJSON = do
  let request = setRequestProxy (Just (Proxy "127.0.0.1" 9041)) botURL
  res <- httpLBS request
  return (getResponseBody res)


lastMsg :: IO String
lastMsg = do
  rawJSON <- fetchJSON
  let result = decode rawJSON :: Maybe Messages
  return $ case result of
    Nothing -> Message "system" "Error"
    Just (Messages js) -> last js

main :: IO ()
main = do
  msg <- lastMsg
  putStrLn $ printPretty msg


printPretty :: Message -> String
printPretty (Message name text) = "name = " ++ name ++ ", text = " ++ text
