{-# LANGUAGE OverloadedStrings #-}

module App where

import           Data.Aeson           (Value (Number))
import qualified Data.Aeson           as Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Digest.CRC32    (CRC32 (..))
import qualified Data.HashMap.Strict  as HashMap
import           Data.Internal        (FRVTResponse (..), Token)
import           Data.Maybe           (fromMaybe)
import qualified Data.Scientific      as Scientific
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text
import           Data.Word            (Word32)
import           Network.HTTP.Types   (RequestHeaders, StdMethod (PUT),
                                       accepted202, mkStatus, parseMethod,
                                       status200, status400, status403,
                                       status404, status405, status500, hAuthorization)
import           Network.Wai          (Application, Request, Response,
                                       lazyRequestBody, rawPathInfo,
                                       requestHeaders, requestMethod,
                                       responseLBS)


app :: Application
app request respond =
    case rawPathInfo request of
        "/" -> respond honeypot
        "/events" -> events request >>= respond
        _ -> respond notFound

honeypot :: Response
honeypot =
    responseLBS
        status200
        [("Content-Type", "text/plain")]
        "What are you doing here, man?"


events :: Request -> IO Response
events request =
    case parseMethod (requestMethod request) of
        Left err ->
            return
                (responseLBS
                     status400
                     [("Content-Type", "application/json")]
                     (Aeson.encode
                          FRVTResponse
                          { body = "Can't parse method"
                          }))
        Right mtd ->
            case mtd of
                PUT ->
                    case tryToExtractToken (requestHeaders request) of
                        Left err ->
                            return
                                (responseLBS
                                     status403
                                     [("Content-Type", "application/json")]
                                     (Aeson.encode
                                          FRVTResponse
                                          { body = "No token"
                                          }))
                        Right res ->
                            lazyRequestBody request >>=
                            \bdy ->
                                 case parseEvent bdy of
                                     Left err ->
                                         return
                                             (responseLBS
                                                  status500
                                                  [ ( "Content-Type"
                                                    , "application/json")]
                                                  (Aeson.encode
                                                       FRVTResponse
                                                       { body = "can't parse event"
                                                       }))
                                     Right event ->
                                         if isNotIntegrity event
                                             then return
                                                      (responseLBS
                                                           (mkStatus
                                                                422
                                                                "Unprocessable Entity")
                                                           [ ( "Content-Type"
                                                             , "application/json")]
                                                           (Aeson.encode
                                                                FRVTResponse
                                                                { body = "Broken integrity"
                                                                }))
                                             else registerEvent
                                                      res
                                                      (eventEntity event) >>
                                                  return
                                                      (responseLBS
                                                           accepted202
                                                           [ ( "Content-Type"
                                                             , "application/json")]
                                                           (Aeson.encode
                                                                FRVTResponse
                                                                { body = "Event registered"
                                                                }))
                _ ->
                    return
                        (responseLBS
                             status405
                             [("Content-Type", "application/json")]
                             (Aeson.encode
                                  FRVTResponse
                                  { body = "Support only PUT method yet."
                                  }))


notFound :: Response
notFound =
    responseLBS status404 [("Content-Type", "text/plain")] "404 - Not Found"


tryToExtractToken :: RequestHeaders -> Either String Token
tryToExtractToken [] = Left "no token"
tryToExtractToken ((header,token):xs) =
    if header == hAuthorization
        then Right token
        else tryToExtractToken xs

parseEvent :: ByteString -> Either String Aeson.Object
parseEvent = Aeson.eitherDecode

isNotIntegrity :: Aeson.Object -> Bool
isNotIntegrity event =
    case HashMap.lookup "crc" event of
        Just (Number crcvalue) ->
            case HashMap.lookup "entities" event of
                Nothing -> True
                Just entities ->
                    case Scientific.toBoundedInteger crcvalue :: Maybe Word32 of
                        Nothing -> True
                        Just crcvalue' ->
                            not (crc32 (Aeson.encode entities) == crcvalue')
        _ -> True

eventEntity :: Aeson.Object -> ByteString
eventEntity = fromMaybe "" . Just . Aeson.encode <$> HashMap.lookup "entities"

registerEvent :: Token -> ByteString -> IO ()
registerEvent _ _ = return ()
