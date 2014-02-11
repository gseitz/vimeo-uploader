{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Vimeo.Types where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text           (Text)
import           GHC.Generics


data ResponseInfo a = ResponseInfo
    { rspGeneratedIn :: Double
    , rspStatus      :: Status
    , rspErrorOrMsg  :: Either VimeoError a
    } deriving Show

data VimeoError = VimeoError
    { veCode :: Int
    , veMsg  :: String
    , veExpl :: Maybe String
    } deriving (Show, Generic)

data Status = Ok | Fail deriving Show

instance FromJSON VimeoError where
    parseJSON (Object o) = do
        err <- o .: "err"
        VimeoError <$>
            parseStringly err "code" <*>
            err .: "msg" <*>
            err .:? "expl"

instance FromJSON a => FromJSON (ResponseInfo a) where
    parseJSON obj@(Object v) = do
        errorOrA <- parseErrorOr obj
        ResponseInfo <$>
            parseStringly v "generated_in" <*>
            v .: "stat" <*>
            return errorOrA

instance FromJSON Status where
    parseJSON (String "ok")   = return Ok
    parseJSON (String "fail") = return Fail


type ResponseOrError a = Either VimeoError a


-- ############### QUOTA

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
    parseJSON (Object obj) = do
        v <- obj .: "user"
        QuotaUser <$>
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

-- ################# UPLOAD

data UploadMethod = Streaming | Post deriving (Show)

data Ticket = Ticket
    { tId             :: String
    , tEndpoint       :: String
    , tEndpointSecure :: String
    , tMaxFileSize    :: Integer
    } deriving (Show)

instance FromJSON Ticket where
    parseJSON (Object obj) = do
        v <- obj .: "ticket"
        Ticket <$>
            v .: "id" <*>
            v .: "endpoint" <*>
            v .: "endpoint_secure" <*>
            parseStringly v "max_file_size"


data TicketStatus = Invalid | Complete | Uploaded Integer deriving (Show)


parseNumericBool :: Object -> Text -> Parser Bool
parseNumericBool obj key = (1 ==) `fmap` (parseStringly obj key :: Parser Int)

parseStringly :: Read a => Object -> Text -> Parser a
parseStringly obj key = read `fmap` (obj .: key)

parseErrorOr :: FromJSON a => Value -> Parser (Either VimeoError a)
parseErrorOr obj@(Object v) = right >>= maybe left (return.Right)
  where
    right = parseJSON obj
    left  = Left `fmap` (v .: "err")

fromEither :: Either a b -> Maybe b
fromEither = either (const Nothing) Just
