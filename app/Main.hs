{-# LANGUAGE OverloadedStrings #-}
module Main where

import           App                      (app)
import           Katip                    (ColorStrategy (ColorIfTerminal),
                                           Severity (DebugS), Verbosity (V3),
                                           initLogEnv, mkHandleScribe,
                                           registerScribe)
import           Network.Wai.Handler.Warp (run)
import           System.IO                (stderr)

main :: IO ()
main = do
    logEnv <- initLogEnv "negotians" "devel"
    scribeHandle <- mkHandleScribe ColorIfTerminal stderr DebugS V3
    run 3000 (app (registerScribe "stderr" scribeHandle logEnv))
