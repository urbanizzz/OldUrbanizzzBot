{-# LANGUAGE OverloadedStrings #-}

module Main where

import            System.IO
import            Network.HTTP.Simple
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
  repeat          :: Integer,
  repeat_question :: String,
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

botURL = "https://api.telegram.org/bot949284451:AAGK8fCgIhv2KLcmT8Mz_bf-3hAl0Ccp7pA/getUpdates?offset=185794573"
botURL1 = "https://api.telegram.org/bot949284451:AAGK8fCgIhv2KLcmT8Mz_bf-3hAl0Ccp7pA/sendMessage?chat_id=845633894&text=fuck you"
fileCfg = "config.json"

readCfg :: IO Config
readCfg = do
  rawJSON <- B.readFile fileCfg
  let result = decode rawJSON :: Maybe Config
  return $ case result of
    Nothing -> Config "UrbanizzzBot" 0 "Enter the number of repetitions" 0
    Just js -> js

{-
writeCfg :: Config -> IO ()
writeCfg cfg = withFile fileCfg WriteMode (\handle -> do
  data <- encode cfg
  B8.hPutStr data)
-}
{-
  bracketOnError
    (openTemplFile "." "temp")
    (\(tempName, tempHandle) -> do
      hClose templHandle
      removeFile tempName)
    (\(tempName, tempHandle) -> do
      B.hPutStr tempHandle cfg
      hClose tempHandle
      renameFile tempName fileCfg)
-}

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
  -- listMsg or lastMsg
  msg <- listMsg
  putStrLn msg
  cfg <- readCfg
  putStrLn $ show cfg
  let cfg1 = Config "qwe" 1 "asd" 0
    in B8.putStrLn $ encode cfg1



printPretty :: Message -> String
printPretty (Message update chat text) =
  "update=" ++ show update ++ ", chat=" ++ show chat ++ ", " ++ text
