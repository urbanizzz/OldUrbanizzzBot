{-# LANGUAGE OverloadedStrings #-}

module Main where

import            System.IO
import            Network.HTTP.Simple
import qualified  Data.ByteString             as BS
import qualified  Data.ByteString.Lazy        as B
import qualified  Data.ByteString.Lazy.Char8  as B8
import            Data.Aeson
import            Data.Aeson.Types            (parseMaybe)
import qualified  Data.Aeson.Encode.Pretty    as Pr
import            Control.Monad               (mzero)
import            Control.Applicative         ((<$>), (<*>))


-- config
data Config = Config {
  about           :: String,
  repeatNumber    :: Integer,
  repeatQuestion  :: String,
  offset          :: Integer }
instance FromJSON Config where
  parseJSON (Object cfg)  = Config
                            <$>
                            cfg .: "about"
                            <*>
                            cfg .: "repeat"
                            <*>
                            cfg .: "repeat_question"
                            <*>
                            cfg .: "offset"
  parseJSON _             = mzero
instance ToJSON Config where
    toJSON (Config ab rep repq off) = object [
                                      "about"           .= ab,
                                      "repeat"          .= rep,
                                      "repeat_question" .= repq,
                                      "offset"          .= off
                                      ]
instance Show Config where
  show (Config ab rep repq off) = "About=" ++ ab ++
                                  "\nNumber of repetitions=" ++ show rep ++
                                  "\nQuestion for repetitions=" ++ repq ++
                                  "\nOffset=" ++ show off

-- single message
data Message = Message  {update_id :: Integer, chat_id :: Integer, text :: String}
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

botURL = "https://api.telegram.org/bot949284451:AAGK8fCgIhv2KLcmT8Mz_bf-3hAl0Ccp7pA/"
fileCfg = "config.json"
defaultCfg  = Config {
                about = "UrbanizzzBot",
                repeatNumber = 1,
                repeatQuestion = "Enter the number of repetitions",
                offset = 0 }
emptyMsg =  Message {
              update_id = 0,
              chat_id = 0,
              text = "" }

readCfg :: IO Config
readCfg = do
  rawJSON <- BS.readFile fileCfg
  -- rawJSON <- withFile fileCfg ReadMode (\handle -> B.hGetContents handle)
  let result = decodeStrict rawJSON :: Maybe Config
  return $ case result of
    Nothing -> defaultCfg
    Just js -> js

--todo bracketsOnError
writeCfg :: Config -> IO ()
writeCfg cfg = withFile fileCfg WriteMode (\handle -> do
  B8.hPutStr handle $ Pr.encodePretty cfg)
{-
  bracketOnError
    (openTemplFile "." "temp")
    (\(tempName, tempHandle) -> do
      hClose templHandle
      removeFile tempName)
    (\(tempName, tempHandle) -> do
      B.hPutStr tempHandle cfg
      hClose tempHandle
      renameFile tempName fileCfg)-}

editCfgOffset :: Integer -> Config -> Config
editCfgOffset off (Config ab rep repq _) = Config ab rep repq off

fetchJSON :: IO B.ByteString
fetchJSON = do
  cfg <- readCfg
  url <- parseRequest $ botURL ++ "getUpdates?offset=" ++ (show . offset $ cfg)
  let request = setRequestProxy (Just (Proxy "127.0.0.1" 9041)) url
  res <- httpLBS request
  return (getResponseBody res)

-- getting last message
lastMsg :: IO Message
lastMsg = do
  rawJSON <- fetchJSON
  let result = decode rawJSON :: Maybe Messages
  return $ case result of
    Nothing -> emptyMsg
    Just (Messages js) -> if null js then emptyMsg else last $ js

repeatMsg :: IO ()
repeatMsg = do
  msg <- lastMsg
  if text msg == ""
    then return ()
    else do
      cfg <- readCfg
      writeCfg $ editCfgOffset (1 + update_id msg) cfg
      sendMsg msg

-- sending message
sendMsg :: Message -> IO ()
sendMsg msg = do
  url <- parseRequest $ botURL ++ "sendMessage?chat_id=" ++ (show . chat_id $ msg) ++ "&text=" ++ (text msg)
  let request = setRequestProxy (Just (Proxy "127.0.0.1" 9041)) $ url
  httpLBS request
  return ()

main :: IO ()
main = do
  -- listMsg
  repeatMsg
  -- cfg <- readCfg
  -- print cfg
  -- writeCfg $ Config "qqq" 10 "Bla-bla" 20

  return ()



printPretty :: Message -> String
printPretty (Message update chat text) =
  "update=" ++ show update ++ ", chat=" ++ show chat ++ ", " ++ text

-- getting list of messages
listMsg :: IO String
listMsg = do
  rawJSON <- fetchJSON
  let result = decode rawJSON :: Maybe Messages
  return $ case result of
    Nothing -> "Error"
    Just (Messages js) -> foldl (\a s -> a ++ "\n" ++ s) "" $ printPretty <$> js
