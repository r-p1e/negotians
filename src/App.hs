{-# LANGUAGE OverloadedStrings #-}

module App where

import           Control.Monad.IO.Class    (MonadIO (liftIO))
import           Crypto.Hash               (Digest, MD5, digestFromByteString,
                                            hash)
import           Data.ByteString           (ByteString)
import           Data.Monoid               ((<>))
import           Data.Text.Encoding        (decodeUtf8)
import           Internal                  (LogMsg (..), describeHeader)
import           Katip                     (KatipT, LogEnv,
                                            Severity (ErrorS, WarningS), logMsg,
                                            ls, runKatipT)
import           Network.HTTP.Types        (RequestHeaders, StdMethod (PUT),
                                            hAuthorization, parseMethod)
import           Network.HTTP.Types.Status (accepted202, status200, status400,
                                            status403, status404, status405,
                                            status422)
import           Network.Wai               (Application, Request, Response,
                                            rawPathInfo, remoteHost,
                                            requestBody, requestHeaders,
                                            requestMethod, responseLBS)
import           Network.Wai.Logger        (showSockAddr)


type Token = ByteString

app :: LogEnv -> Application
app logEnv request respond =
    case rawPathInfo request of
        "/" ->
            runKatipT
                logEnv
                (logMsg
                     "app"
                     WarningS
                     (ls
                          LogMsg
                          { who = showSockAddr (remoteHost request)
                          , circumstances = decodeUtf8 $
                            requestMethod request <> " " <> rawPathInfo request
                          , what = "honeypot"
                          })) >>
            respond honeypot
        "/events" -> runKatipT logEnv (events request) >>= respond
        _ -> runKatipT
                logEnv
                (logMsg
                     "app"
                     ErrorS
                     (ls
                          LogMsg
                          { who = showSockAddr (remoteHost request)
                          , circumstances = decodeUtf8 $
                            requestMethod request <> " " <> rawPathInfo request
                          , what = "not found"
                          })) >> respond notFound

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

extractCheckSum :: Request -> Either Response (Digest MD5)
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
checkIntegrity :: Digest MD5 -> Digest MD5 -> Either Response Response
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

events :: Request -> KatipT IO Response
events request =
    case checkAuth request of
        Left err ->
            logMsg
                "app"
                ErrorS
                (ls
                     LogMsg
                     { who = showSockAddr (remoteHost request)
                     , circumstances = describeHeader $ requestHeaders request
                     , what = "check authorization"
                     }) >>
            return err
        Right token -> do
            reqbody <- liftIO $ requestBody request
            case (checkMethod request >> extractCheckSum request >>=
                  checkIntegrity (calculateMD5 reqbody)) of
                Left response ->
                    logMsg
                        "app"
                        ErrorS
                        (ls
                             LogMsg
                             { who = showSockAddr (remoteHost request)
                             , circumstances = describeHeader $
                               requestHeaders request
                             , what = "check method or other"
                             }) >>
                    return response
                Right response ->
                    registerEvent token reqbody >> return response

calculateMD5 :: ByteString -> Digest MD5
calculateMD5 = hash

notFound :: Response
notFound =
    responseLBS status404 [("Content-Type", "plain/text")] "not found - 404"

tryToExtractToken :: RequestHeaders -> Either String Token
tryToExtractToken [] = Left "no token"
tryToExtractToken ((header,token):xs) =
    if header == hAuthorization
        then Right token
        else tryToExtractToken xs

tryToExtractContentMD5 :: RequestHeaders -> Either String (Digest MD5)
tryToExtractContentMD5 [] = Left "no content-md5 header"
tryToExtractContentMD5 (("content-md5",checksum):_) =
    case digestFromByteString checksum of
        Nothing -> Left "broken md5 hash"
        Just digest -> Right digest
tryToExtractContentMD5 (_:xs) = tryToExtractContentMD5 xs

registerEvent :: Token -> ByteString -> KatipT IO ()
registerEvent _ _ = return ()
