{-# LANGUAGE OverloadedStrings #-}

module Main where

import            Network.HTTP.Simple
import qualified  Data.ByteString.Lazy    as B
import            Data.Aeson
import            Data.Aeson.Types        (parseMaybe)
import            Control.Monad           (mzero)
import            Control.Applicative     ((<$>), (<*>))

-- single message
data Message = Message  { update_id :: Integer, chat_id :: Integer, text :: String }
instance FromJSON Message where
  parseJSON (Object msg)  = Message
                            <$>
                            -- extract update_id
                            (msg .: "update_id")
                            <*>
                            -- extracting chat_id (message -> chat -> id)
                            (msg .: "message" >>= (.: "chat") >>= (.: "id"))
                            <*>
                            -- extracting text of a message (message -> text)
                            (msg .: "message" >>= (.: "text"))
  parseJSON _             = mzero

-- list of messages
data Messages = Messages [Message]
instance FromJSON Messages where
  parseJSON (Object msgs) = Messages <$> msgs .: "result"

botURL = "https://api.telegram.org/bot949284451:AAGK8fCgIhv2KLcmT8Mz_bf-3hAl0Ccp7pA/getUpdates?offset=185794573"
botURL1 = "https://api.telegram.org/bot949284451:AAGK8fCgIhv2KLcmT8Mz_bf-3hAl0Ccp7pA/sendMessage?chat_id=845633894&text=fuck you"

fetchJSON :: IO B.ByteString
fetchJSON = do
  let request = setRequestProxy (Just (Proxy "127.0.0.1" 9041)) $ botURL
  res <- httpLBS request
  return (getResponseBody res)


-- getting last message
lastMsg :: IO String
lastMsg = do
  rawJSON <- fetchJSON
  let result = decode rawJSON :: Maybe Messages
  return $ case result of
    Nothing -> "Error"
    Just (Messages js) -> if null js then "" else printPretty . last $ js

-- getting list of messages
listMsg :: IO String
listMsg = do
  rawJSON <- fetchJSON
  let result = decode rawJSON :: Maybe Messages
  return $ case result of
    Nothing -> "Error"
    Just (Messages js) -> foldl (\a s -> a ++ "\n" ++ s) "" $ printPretty <$> js

sendMsg :: IO ()
sendMsg = do
  let request = setRequestProxy (Just (Proxy "127.0.0.1" 9041)) $ botURL1
  httpLBS request
  return ()

main :: IO ()
main = do
  sendMsg
  -- listMsg or lastMsg
  msg <- listMsg
  putStrLn msg


printPretty :: Message -> String
printPretty (Message update chat text) =
  "update=" ++ show update ++ ", chat=" ++ show chat ++ ", " ++ text
