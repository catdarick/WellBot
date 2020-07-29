module Bot.ErrorException where

import           Control.Exception

data ErrorException
  = CommonException String
  | BadTokenException
  | NoConnectionException
  | BadResponseException
  deriving (Eq)

instance Show ErrorException where
  show (CommonException error) = error
  show BadTokenException =
    "Authorization failed. Check your auth data in config file."
  show NoConnectionException = "No connection. Another attempt in 10 seconds."
  show BadResponseException = "Can't parse response."

instance Exception ErrorException
