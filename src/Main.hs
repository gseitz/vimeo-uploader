{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.Reader
import           Data.Data
import           System.Console.CmdArgs

import           Prelude                hiding (quot)

import           Vimeo.API
import           Vimeo.OAuth
import           Vimeo.Types
import           Vimeo.Upload

data VUP = Upload { file :: FilePath, debug :: Bool }
         | Quota  { short :: Bool, debug :: Bool }
    deriving (Data,Typeable,Show)

upload :: VUP
upload = Upload { file = def, debug = False }

quota :: VUP
quota = Quota False False &= auto

printQuota :: Bool -> OAuthM ()
printQuota s = do
    resp <- getQuota
    liftIO $ putStrLn $ case resp of
        Left parseError -> "Error parsing response: " ++ parseError
        Right quotaMsg -> either show msg $ rspErrorOrMsg quotaMsg
  where
    msg quot = if s
        then (show . usFree . qrUploadSpace) quot
        else let intprint f = (show.f.qrUploadSpace) quot
             in "Total: " ++ intprint usMax ++
                "\nUsed: " ++ intprint usUsed ++
                "\nFree: "++ intprint usFree

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
    command (Quota s dbg) = (dbg, printQuota s)
    command (Upload f dbg) = (dbg, runUpload f)
    (a,b) |+ c = (a,b,c)
