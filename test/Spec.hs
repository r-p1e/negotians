{-# LANGUAGE OverloadedStrings #-}

import           App            (app)
import           Katip          (LogEnv, initLogEnv)
import           Test.Hspec     (Spec, describe, hspec, it)
import           Test.Hspec.Wai (get, request, shouldRespondWith, with)

main :: IO ()
main =
    initLogEnv "negotians" "test" >>=
    \le ->
         hspec (spec le)


spec :: LogEnv -> Spec
spec logEnv =
    with (return (app logEnv)) $
    do describe "GET /" $
           do it "responds with 200" $ do get "/" `shouldRespondWith` 200
       describe "GET /notfound" $
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
              it "responds with 202" $
                  do (request
                          "PUT"
                          "/events"
                          [ ("Authorization", "wow123")
                          , ("Content-MD5", "78b9d09661da64f0bc6c146c524bae4a")]
                          "somebody") `shouldRespondWith`
                         202
