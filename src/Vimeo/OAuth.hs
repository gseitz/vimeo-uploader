module Vimeo.OAuth where

import qualified Data.ByteString        as B

import           Control.Applicative
import           Control.Monad.Reader
import           Network.HTTP.Conduit
import           System.IO
import           Web.Authenticate.OAuth


type OAuthM a = ReaderT (OAuth,Credential,Bool) IO a
type Keys = (B.ByteString,B.ByteString,B.ByteString,B.ByteString)

readOAuthConfig :: FilePath -> IO Keys
readOAuthConfig file = withFile file ReadMode $ \h ->
    (,,,) <$> B.hGetLine h <*> B.hGetLine h <*> B.hGetLine h <*> B.hGetLine h

makeOAuth :: Keys -> (OAuth, Credential)
makeOAuth (consKey, consSecret, accTk, accSecret) = (oauth, creds)
  where
    oauth = newOAuth { oauthServerName = "vimeo"
                     , oauthConsumerKey = consKey
                     , oauthConsumerSecret = consSecret
                     }
    creds = newCredential accTk accSecret

signRequest :: Request -> OAuthM (IO Request)
signRequest req = do
    (vupOauth, creds, _) <- ask
    return $ signOAuth vupOauth creds req
