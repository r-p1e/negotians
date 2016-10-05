{-# LANGUAGE OverloadedStrings #-}
module Main where

import           App                                       (app)
import           Data.Default                              (def)
import           Internal                                  (NtsConfig (..))
import           Network.Wai.Handler.Warp                  (run)
import           Network.Wai.Middleware.Gzip               (gzip)
import           Network.Wai.Middleware.RequestLogger      (OutputFormat (CustomOutputFormatWithDetails),
                                                            mkRequestLogger,
                                                            outputFormat)
import           Network.Wai.Middleware.RequestLogger.JSON (formatAsJSON)
import           Output                                    (initOutput)


main :: IO ()
main = do
    outputFunc <- initOutput
    logStdout <-
        mkRequestLogger
            def
            { outputFormat = CustomOutputFormatWithDetails formatAsJSON
            }
    run 3000 $ logStdout $ gzip def $ app (NtsConfig outputFunc)
