{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.Reader
import           Data.Data
import           System.Console.CmdArgs

import           Vimeo.API
import           Vimeo.OAuth
import           Vimeo.Types
import           Vimeo.Upload

data VUP = Upload { file :: FilePath, debug :: Bool }
         | Quota  { debug :: Bool }
    deriving (Data,Typeable,Show)

upload :: VUP
upload = Upload { file = def, debug = False }

quota :: VUP
quota = Quota False &= auto

printQuota :: OAuthM ()
printQuota = do
    resp <- getQuota
    liftIO $ print resp

runUpload :: FilePath -> OAuthM ()
runUpload videoFile = do
    ticket <- getTicket Streaming
    case ticket of
        Left err -> liftIO $ print $ "Error processing the response from the server: " ++ show err
        Right info@(ResponseInfo _ _ (Left _)) ->
            liftIO $ print $ "Error: " ++ show info
        Right (ResponseInfo _ _ (Right tick)) -> do
            info <- liftIO $ mkUpload tick videoFile
            loop info 0
  where
    loop up offset = do
        uploadFile up offset
        uploadedOpt <- verifyTicket up
        case uploadedOpt of
            Complete -> do
                void $ completeUpload up
                liftIO $ putStrLn "Upload successful!"
            Uploaded uploaded -> loop up (uploaded+1)
            Invalid -> liftIO $ putStrLn "Couldn't parse verification response"

main :: IO ()
main = do
    cmd <- cmdArgs (modes [upload, quota])
    cfg <- makeOAuth `fmap` readOAuthConfig ".vuprc"
    let (dbg, action) = command cmd
    void $ runReaderT action (cfg|+dbg)
  where
    command (Quota dbg) = (dbg, printQuota)
    command (Upload f dbg) = (dbg, runUpload f)
    (a,b) |+ c = (a,b,c)
