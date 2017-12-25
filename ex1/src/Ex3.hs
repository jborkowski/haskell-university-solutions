module Ex3
  (foreverThreeTimes
  ) where

import Control.Monad
import Ex2

foreverThreeTimes :: IO ()
foreverThreeTimes = forever
  greeting
