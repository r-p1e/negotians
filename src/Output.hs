
module Output
    ( initOutput
    ) where

import Data.Text (Text)
import Output.GooglePubSub (initGPS)
import Proto.CommonLogRep (LogEntry)


initOutput
    :: IO (LogEntry -> IO ())
initOutput = initGPS
