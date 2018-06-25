module Task
  ( Task (..)
  ) where

import Data.Time

data Task
  = Task { summary :: String
         , due     :: UTCTime
         } deriving (Read, Show)
