{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Vimeo.Types where

import           Control.Applicative

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text           (Text)

import           GHC.Generics


data ResponseInfo = ResponseInfo
    { rspGeneratedIn :: Double
    , rspStatus      :: Status
    } deriving Show

data VimeoError = VimeoError
    { code :: Int
    , msg  :: Maybe String
    , expl :: Maybe String
    } deriving (Show, Generic)

data Status = Ok | Fail deriving Show

instance FromJSON VimeoError where
    parseJSON (Object v) = VimeoError <$>
        parseStringly v "code" <*>
        v .:? "msg" <*>
        v .:? "expl"

instance FromJSON ResponseInfo where
    parseJSON (Object v) = ResponseInfo <$>
        parseStringly v "generated_in" <*>
        v .: "stat"

instance FromJSON Status where
    parseJSON (String "ok")   = return Ok
    parseJSON (String "fail") = return Fail

data QuotaResponse = QuotaResponse
    { qrInfo :: ResponseInfo
    , qrUser :: Either VimeoError QuotaUser
    } deriving (Show, Generic)

data QuotaUser = QuotaUser
    { quId          :: String
    , qrIsPlus      :: Bool
    , qrIsPro       :: Bool
    , qrUploadSpace :: UploadSpace
    , qrHdQuota     :: Double
    , qrSdQuota     :: Double
    } deriving (Show, Generic)

data UploadSpace = UploadSpace
    { usFree   :: Integer
    , usMax    :: Integer
    , usResets :: Double
    , usUsed   :: Double
    } deriving (Show, Generic)

instance FromJSON QuotaUser where
    parseJSON (Object v) = QuotaUser <$>
        v .: "id" <*>
        parseNumericBool v "is_plus" <*>
        parseNumericBool v "is_pro" <*>
        v .: "upload_space" <*>
        parseStringly v "hd_quota" <*>
        parseStringly v "sd_quota"

instance FromJSON UploadSpace where
    parseJSON (Object v) = UploadSpace <$>
        parseStringly v "free" <*>
        parseStringly v "max" <*>
        parseStringly v "resets" <*>
        parseStringly v "used"

instance FromJSON QuotaResponse where
    parseJSON obj@(Object v) = QuotaResponse <$>
        parseJSON obj <*>
        v .: "user"


data UploadMethod = Streaming | Post deriving (Show)

data GetTicketResponse = GetTicketResponse
    { trResponseInfo :: ResponseInfo
    , trTicket :: Either VimeoError Ticket
    } deriving (Show)

data Ticket = Ticket
    { tId :: String
    , tEndpoint :: String
    , tEndpointSecure :: String
    , tMaxFileSize :: Integer
    } deriving (Show)

instance FromJSON GetTicketResponse where
    parseJSON obj@(Object v) = GetTicketResponse <$>
        parseJSON obj <*>
        parseErrorOr v "ticket"

instance FromJSON Ticket where
    parseJSON (Object v) = Ticket <$>
        v .: "id" <*>
        v .: "endpoint" <*>
        v .: "endpoint_secure" <*>
        parseStringly v "max_file_size"




parseNumericBool :: Object -> Text -> Parser Bool
parseNumericBool obj key = (1 ==) `fmap` parseStringly obj key

parseStringly :: Read a => Object -> Text -> Parser a
parseStringly obj key = read `fmap` (obj .: key)

parseErrorOr :: FromJSON a => Object -> Text -> Parser (Either VimeoError a)
parseErrorOr obj key = right >>= maybe left (return.Right)
  where
    right = obj .:? key
    left  = Left `fmap` (obj .: "err")
