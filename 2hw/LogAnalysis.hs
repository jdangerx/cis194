{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s = case words s of
                   "E":level:time:msg -> LogMessage (Error (read level :: Int)) (read time :: Int) (unwords msg)
                   "I":time:msg -> LogMessage Info (read time :: Int) (unwords msg)
                   "W":time:msg -> LogMessage Warning (read time :: Int) (unwords msg)
                   text -> Unknown (unwords text)
-- TODO: handle malformed strings such as "E 12 NaN 1230 message"
-- which doesn't occur in actual logs - testing never crashes out

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert log Leaf = Node Leaf log Leaf
insert msg@(LogMessage _ ts _) tree@(Node left root@(LogMessage _ rootts _) right)
       | ts < rootts = Node (insert msg left) root right
       | ts >= rootts = Node left root (insert msg right)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (msg:msgs) = insert msg (build msgs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left root right) = inOrder left ++ [root] ++ inOrder right

isGraveError :: LogMessage -> Bool
isGraveError (LogMessage (Error level) _ _) = level >= 50
isGraveError _ = False

isMustardRelated :: LogMessage -> Bool
isMustardRelated (LogMessage _ ts _) = (3410 < ts) && (ts < 3450)
isMustardRelated _ = False

getMessageFromLog :: LogMessage -> String
getMessageFromLog (LogMessage _ _ msg) = msg
getMessageFromLog (Unknown _ ) = ""

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong logs = map getMessageFromLog (filter isGraveError (inOrder (build logs)))

getMustardLogs :: [LogMessage] -> [String]
getMustardLogs [] = []
getMustardLogs logs = map getMessageFromLog (filter isMustardRelated (inOrder (build logs)))
