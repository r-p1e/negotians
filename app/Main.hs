{-# LANGUAGE OverloadedStrings #-}
module Main where

import           App                      (app)
import           Internal                 (NtsConfig (..))
import           Network.Wai.Handler.Warp (run)
import           Output                   (initOutput)

main :: IO ()
main = do
    outputFunc <- initOutput
    run 3000 (app (NtsConfig outputFunc))
