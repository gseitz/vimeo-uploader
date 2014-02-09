module Vimeo.API where


import           Vimeo.HTTP
import           Vimeo.OAuth
import           Vimeo.Types

endpoint :: String
endpoint = "http://vimeo.com/api/rest/v2?format=json&method="

(&) :: String -> String -> String
l & r = l ++ "&" ++ r

getQuota :: OAuthM (Either String (ResponseInfo QuotaUser))
getQuota = decodeResponse $ endpoint ++ "vimeo.videos.upload.getQuota"

getTicket :: UploadMethod -> OAuthM (Either String (ResponseInfo Ticket))
getTicket upmeth = decodeResponse url
  where
    url = endpoint ++ "vimeo.videos.upload.getTicket&upload_method=" ++ meth
    meth = case upmeth of
        Post -> "post"
        Streaming -> "streaming"
