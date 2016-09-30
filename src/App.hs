{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

import           Control.Monad.IO.Class    (MonadIO (liftIO))
import           Crypto.Hash               (Digest, MD5, hash)
import           Data.ByteString           (ByteString)
import           Data.ProtoLens.Encoding   (decodeMessage)
import           Internal                  (NtsConfig (..), Token)
import           Lens.Family2              ((^.))
import           Network.HTTP.Types        (RequestHeaders, StdMethod (PUT),
                                            hAuthorization, parseMethod)
import           Network.HTTP.Types.Status (accepted202, status200, status400,
                                            status403, status404, status405,
                                            status422)
import           Network.Wai               (Application, Request, Response,
                                            rawPathInfo, requestBody,
                                            requestHeaders, requestMethod,
                                            responseLBS)
import           Proto.EventLog            (EventLogs, entities, msg, severity,
                                            source, timestamp)

import qualified Data.ByteArray.Encoding   as B (Base (Base16), convertToBase)



app :: NtsConfig -> Application
app cfg request respond =
    case rawPathInfo request of
        "/" ->
            respond honeypot
        "/events" ->
            events cfg request >>= respond
        _ ->
            respond notFound

honeypot :: Response
honeypot =
    responseLBS
        status200
        [("Content-Type", "text/plain")]
        "What are you doing here, man?"

checkMethod :: Request -> Either Response ()
checkMethod request =
    case parseMethod (requestMethod request) of
        Left err ->
            Left
                (responseLBS
                     status400
                     [("Content-Type", "plain/text")]
                     "Unknown method")
        Right mtd ->
            case mtd of
                PUT -> Right ()
                _ ->
                    Left
                        (responseLBS
                             status405
                             [("Content-Type", "plain/text")]
                             "Only PUT supported")

checkAuth :: Request -> Either Response Token
checkAuth request =
    case tryToExtractToken (requestHeaders request) of
        Left _err ->
            Left
                (responseLBS
                     status403
                     [("Content-Type", "plain/text")]
                     "There is no authorization token")
        Right res -> Right res

extractCheckSum :: Request -> Either Response ByteString
extractCheckSum request =
    case tryToExtractContentMD5 (requestHeaders request) of
        Left _ ->
            Left
                (responseLBS
                     status422
                     [("Content-Type", "plain/text")]
                     "Content-MD5 header is required")
        Right res -> Right res

-- | Check that Content-MD5 and request body md5 hash is equal
checkIntegrity :: ByteString -> ByteString -> Either Response Response
checkIntegrity rbodymd5 contentmd5 =
    case contentmd5 == rbodymd5 of
        False ->
            Left
                (responseLBS
                     status422
                     [("Content-Type", "plain/text")]
                     "Checksum Failed")
        True ->
            Right
                (responseLBS
                     accepted202
                     [("Content-Type", "plain/text")]
                     "Accepted")

events
    :: NtsConfig -> Request -> IO Response
events cfg request =
    case checkAuth request of
        Left err -> return err
        Right token -> do
            reqbody <- liftIO $ requestBody request
            case (checkMethod request >> extractCheckSum request >>=
                  checkIntegrity (calculateMD5 reqbody)) of
                Left response -> return response
                Right response ->
                    case decodeMessage reqbody :: Either String EventLogs of
                        Left err ->
                            return
                                (responseLBS
                                     status400
                                     [("Content-Type", "plain/text")]
                                     "Can't decode message")
                        Right msg' ->
                            mapM_
                                (\entity ->
                                      (output cfg)
                                          token
                                          (entity ^. timestamp)
                                          (entity ^. severity)
                                          (entity ^. source)
                                          (entity ^. msg))
                                (msg' ^. entities) >>
                            return response

calculateMD5 :: ByteString -> ByteString
calculateMD5 value = B.convertToBase B.Base16 (hash value :: Digest MD5)

notFound :: Response
notFound =
    responseLBS status404 [("Content-Type", "plain/text")] "not found - 404"

tryToExtractToken :: RequestHeaders -> Either String Token
tryToExtractToken [] = Left "no token"
tryToExtractToken ((header,token):xs) =
    if header == hAuthorization
        then Right token
        else tryToExtractToken xs

tryToExtractContentMD5 :: RequestHeaders -> Either String ByteString
tryToExtractContentMD5 [] = Left "no content-md5 header"
tryToExtractContentMD5 (("content-md5",checksum):_) = Right checksum
tryToExtractContentMD5 (_:xs) = tryToExtractContentMD5 xs
