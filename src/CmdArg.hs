module CmdArg where

import Data.Time

import Task

data CmdArg
  = CmdUnknown
  | CmdNotProvided
  | CmdTask String
  | CmdDue UTCTime
  | CmdList
  | CmdRemove Int
  deriving (Read, Show)

-- True when the string starts with "--", False otherwise.
arg :: String -> Bool
arg ('-' : '-' : _) = True
arg _ = False

-- Pair arguments with a list of their parameters
-- collectParams $ words "--a1 p1 p2 --a2 p3"
--   ==> [ ("--a1", [ "p1", "p2" ])
--       , ("--a2", [ "p3" ])
-- Ignores all parameters which come before any arguments, thus
-- collectParams $ words "ignored --t kept"
--  ==> [ ("--t", [ "kept" ]) ]
collectParams :: [String] -> [(String, [String])]
collectParams (x : xs)
  | arg x = let (params, rest) = collectParam xs [] in (x, params) : collectParams rest
  where collectParam [] acc               = (reverse acc, [])
        collectParam (x : xs) acc | arg x = (reverse acc, x : xs)
        collectParam (x : xs) acc         = collectParam xs $ x : acc

-- Convert the output of collectParams into a list of CmdArgs
parseArgs [] = []
parseArgs ((arg, params) : xs) = case arg of
  "--task"   -> (CmdTask $ unwords params) : parseArgs xs
  "--due"    -> let (ymd : hm : []) = params
                    time = parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d %R" (ymd ++ " " ++ hm)
                in (CmdDue time) : parseArgs xs
  "--list"   -> let [] = params in CmdList : parseArgs xs
  "--remove" -> let (taskID : []) = params in CmdRemove (read taskID) : parseArgs xs
