{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           App                       (app, calculateMD5)
import           Data.Binary.Builder       (toLazyByteString)
import           Data.ByteString.Lazy      (toStrict)
import           Data.ProtoLens            (buildMessage, def)
import           Data.ProtoLens.Arbitrary  (ArbitraryMessage (..))
import           Internal                  (NtsConfig (..))
import           Lens.Family2              ((&), (.~))
import           Proto.EventLog            (EventLog, EventLogs,
                                            Severity (INFO), entities, msg,
                                            severity, source, timestamp)
import           Test.Hspec                (Spec, describe, hspec, it)
import           Test.Hspec.Wai            (get, request, shouldRespondWith,
                                            with)
import           Test.Hspec.Wai.QuickCheck (property)


main :: IO ()
main = hspec spec

outputMock _ _ _ _ _ = return ()


spec :: Spec
spec =
    with (return (app (NtsConfig outputMock))) $
    do describe "GET /notfound" $
           do it "responds with 404" $
                  do get "/notfound" `shouldRespondWith` 404
       describe "GET /events" $
           do it "responds with 405 cause do not support GET" $
                  do (request
                          "GET"
                          "/events"
                          [("Authorization", "wow123")]
                          "somebody") `shouldRespondWith`
                         405
              it "responds with 422 cause there is no checksum" $
                  do (request
                          "PUT"
                          "/events"
                          [("Authorization", "wow123")]
                          "somebody") `shouldRespondWith`
                         422
              it "responds with 422 cause checksum is wrong" $
                  do (request
                          "PUT"
                          "/events"
                          [("Authorization", "wow123"), ("Content-MD5", "0")]
                          "somebody") `shouldRespondWith`
                         422
              it "responds with 400" $
                  do (request
                          "PUT"
                          "/events"
                          [ ("Authorization", "wow123")
                          , ("Content-MD5", "78b9d09661da64f0bc6c146c524bae4a")]
                          "somebody") `shouldRespondWith`
                         400
              it "responds with 202" $
                  do let rmsg :: EventLog =
                             ((((def & severity .~ INFO) & timestamp .~ 0) &
                               msg .~
                               "some example text") &
                              source .~
                              "test")
                         body :: EventLogs = (def & entities .~ [rmsg])
                         rbody = toLazyByteString (buildMessage body)
                     (request
                          "PUT"
                          "/events"
                          [ ("Authorization", "wow123")
                          , ("Content-MD5", (calculateMD5 (toStrict rbody)))
                          , ("Content-Type", "application/x-protobuf")]
                          rbody) `shouldRespondWith`
                         202
              it "respond with 202" $
                  property $
                  \(eventlog :: ArbitraryMessage EventLog) ->
                       (let body =
                                toLazyByteString
                                    (buildMessage
                                         (def & entities .~
                                          [unArbitraryMessage eventlog] :: EventLogs))
                        in request
                               "PUT"
                               "/events"
                               [ ("Authorization", "quickchecktoken")
                               , ("Content-MD5", (calculateMD5 (toStrict body)))
                               , ("Content-Type", "application/x-protobuf")]
                               body) `shouldRespondWith`
                       202
