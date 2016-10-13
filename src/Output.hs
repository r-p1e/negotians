
module Output
    ( initOutput
    ) where

import Data.Text (Text)
import Internal (Msg, Source, Token)
import Output.GooglePubSub (initGPS)
import Proto.EventLog (Severity)


initOutput
    :: IO (Token -> Double -> Severity -> Source -> Msg -> IO ())
initOutput = initGPS
