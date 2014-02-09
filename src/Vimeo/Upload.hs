{-# LANGUAGE OverloadedStrings, FlexibleContexts, RecordWildCards, ScopedTypeVariables #-}
module Vimeo.Upload where

import qualified Control.Exception.Lifted as EX
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit.Binary as CB
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import Control.Exception
import Control.Failure
import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import Data.Monoid ((<>))
import Network.HTTP.Conduit
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import System.IO

import Vimeo.API
import Vimeo.HTTP
import Vimeo.OAuth
import Vimeo.Types


mkUploadRequest :: UploadInfo -> Integer -> Maybe Integer -> IO Request
mkUploadRequest (UploadInfo ticket file size) offset maxSize= do
    initReq <- parseUrl $ tEndpointSecure ticket
    return $ initReq { method = methodPut
                      , secure = True
                      , responseTimeout = Nothing
                      , requestBody = requestBodySource (fromInteger uploadSize) $
                            CB.sourceFileRange file (Just offset) (Just uploadSize)
                      , requestHeaders = ("Content-Type", "video/mp4") : contentRange
                      }
  where
    remaining = size-offset
    uploadSize = maybe remaining (min remaining) maxSize
    rangeEnd = offset + uploadSize - 1
    contentRangeValue = "bytes " <> s2b offset <> "-" <> s2b rangeEnd <> "/" <> s2b size
    s2b = E.encodeUtf8 . T.pack . show
    contentRange = if uploadSize == size
        then []
        else [("Content-Range", contentRangeValue)]

mkVerifyRequest :: Failure HttpException m => Ticket -> m Request
mkVerifyRequest ticket = do
    initReq <- parseUrl $ tEndpoint ticket
    return $ initReq { method = methodPut
                      , requestHeaders =
                            [ ("Content-Length", "0")
                            , ("Content-Range", "bytes */*")
                            ]
                      , checkStatus = checkUploadStatus
                      }
  where
    checkUploadStatus (Status 308 _) _ _ = Nothing
    checkUploadStatus s hs cj = Just $ toException $ StatusCodeException s hs cj

verifyTicket :: UploadInfo -> OAuthM TicketStatus
verifyTicket (UploadInfo ticket _ size) = do
    req <- mkVerifyRequest ticket
    response <- executeRequest req
    let headers = responseHeaders response
        status = flip fmap (uploaded headers) $ \up ->
            if up == size-1
                then Complete
                else Uploaded up
    return $ fromMaybe Invalid status
  where
    rangeHeader :: ResponseHeaders -> Maybe T.Text
    rangeHeader headers = E.decodeUtf8 `fmap` lookup "Range" headers
    uploaded headers = (read . T.unpack . last . T.split (=='-')) `fmap` rangeHeader headers

completeUpload :: UploadInfo -> OAuthM BL.ByteString
completeUpload (UploadInfo (Ticket {..}) file _) = extractBody request
  where
    request = endpoint ++ "vimeo.videos.upload.complete" & ticket_id & filename
    ticket_id = "ticket_id=" ++ tId
    filename = "filename=" ++ file

data UploadInfo = UploadInfo
    { upTicket :: Ticket
    , upFile   :: FilePath
    , upSize   :: Integer
    }

mkUpload :: Ticket -> FilePath -> IO UploadInfo
mkUpload ticket file = do
    h <- openBinaryFile file ReadMode
    size <- hFileSize h
    return $ UploadInfo ticket file size

uploadFile :: UploadInfo -> Integer -> OAuthM ()
uploadFile upload offset = do
    req <- liftIO $ mkUploadRequest upload offset (Just 100000)
    EX.catch (void $ executeRequest req)
        (\(ex::SomeException) -> liftIO $ print ex)

