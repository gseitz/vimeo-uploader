module Vimeo.HTTP where

import qualified Data.ByteString.Lazy as BL

import           Control.Monad.Reader
import           Data.Aeson
import           Network.HTTP.Conduit

import           Vimeo.OAuth

decodeResponse :: FromJSON a => String -> OAuthM (Either String a)
decodeResponse url = eitherDecode `fmap` extractBody url


extractBody :: String -> OAuthM BL.ByteString
extractBody url = responseBody `fmap` parsecuteSignedRequest url


parsecuteSignedRequest :: String -> OAuthM (Response BL.ByteString)
parsecuteSignedRequest url = do
    req <- parseUrl url
    executeSignedRequest req

executeSignedRequest :: Request -> OAuthM (Response BL.ByteString)
executeSignedRequest req = do
    signedIO <- signRequest req
    signed <- liftIO signedIO
    executeRequest signed

parsecuteRequest :: String -> OAuthM (Response BL.ByteString)
parsecuteRequest url = do
    req <- parseUrl url
    executeRequest req

executeRequest :: Request -> OAuthM (Response BL.ByteString)
executeRequest req = withManager $ \m -> do
    (_,_,debug) <- ask
    when debug $ liftIO $ do
        putStrLn "\n###### REQUEST ######"
        print req
    resp <- httpLbs req m
    when debug $ liftIO $ do
        putStrLn "\n###### RESPONSE ######"
        print resp
    return resp

