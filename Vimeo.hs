{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Vimeo where

import Control.Applicative
import Control.Monad (void)
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy
import qualified Data.ByteString as B
import Data.Text (Text)
import Network.HTTP.Conduit
import System.IO
import Web.Authenticate.OAuth

import GHC.Generics

import Vimeo.Types

endpoint :: String
endpoint = "http://vimeo.com/api/rest/v2?format=json&method="

readOAuthConfig :: FilePath -> IO (B.ByteString,B.ByteString,B.ByteString,B.ByteString)
readOAuthConfig file = withFile file ReadMode $ \h ->
    (,,,) <$> B.hGetLine h <*> B.hGetLine h <*> B.hGetLine h <*> B.hGetLine h

makeOAuth :: (B.ByteString,B.ByteString,B.ByteString,B.ByteString) -> (OAuth, Credential)
makeOAuth (consKey, consSecret, accTk, accSecret) = (oauth, creds)
  where
    oauth = newOAuth { oauthServerName = "vimeo"
                     , oauthConsumerKey = consKey
                     , oauthConsumerSecret = consSecret 
                     }
    creds = newCredential accTk accSecret

getQuota :: OAuthM (Either String QuotaResponse)
getQuota = decodeRequest $ endpoint ++ "vimeo.videos.upload.getQuota"

getTicket :: UploadMethod -> OAuthM (Either String GetTicketResponse)
getTicket upmeth = decodeRequest url
  where
    url = endpoint ++ "vimeo.videos.upload.getTicket&upload_method=" ++ meth
    meth = case upmeth of
        Post -> "post"
        Streaming -> "streaming"

type OAuthM a = ReaderT (OAuth,Credential) IO a

decodeRequest :: FromJSON a => String -> OAuthM (Either String a)
decodeRequest url = do
    resp <- runRequest url
    return $ eitherDecode resp

runRequest :: String -> OAuthM ByteString
runRequest url = do
  req <- parseUrl url
  (vupOauth, creds) <- ask
  res <- withManager $ \m -> do
    signedreq <- signOAuth vupOauth creds req
    httpLbs signedreq m
  return $ responseBody res



main :: IO ()
main = do
    cfg <- makeOAuth `fmap` readOAuthConfig ".vuprc"
    void $ flip runReaderT cfg $ do
        ticket <- getTicket Streaming 
        liftIO $ print ticket
