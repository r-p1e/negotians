{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           App                                       (app)
import           Data.Default                              (def)
import           Data.Text                                 (Text)
import           GHC.Generics                              (Generic)
import           Internal                                  (NtsConfig (..))
import           Network.Wai.Handler.Warp                  (run)
import           Network.Wai.Middleware.Gzip               (gzip)
import           Network.Wai.Middleware.RequestLogger      (OutputFormat (CustomOutputFormatWithDetails),
                                                            mkRequestLogger,
                                                            outputFormat)
import           Network.Wai.Middleware.RequestLogger.JSON (formatAsJSON)
import           Options.Generic                           (getRecord, ParseRecord, type (<?>)(..))
import           Output                                    (initOutput)


data ArgOpts = ArgOpts
    { amqpUsername :: Text <?> "AMQP user name"
    , amqpPwd      :: Text <?> "AMQP user password"
    } deriving (Generic)

instance ParseRecord ArgOpts

main :: IO ()
main = do
    argopts <-
        getRecord
            "Negotians - server for collect logs from agent and send further"
    outputFunc <-
        initOutput
            (unHelpful (amqpUsername argopts))
            (unHelpful (amqpPwd argopts))
    logStdout <-
        mkRequestLogger
            def
            { outputFormat = CustomOutputFormatWithDetails formatAsJSON
            }
    run 3000 $ logStdout $ gzip def $ app (NtsConfig outputFunc)
