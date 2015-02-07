{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1: parse an individual message.
-- TODO: handle malformed strings such as "E 12 NaN 1230 message"
-- which doesn't occur in actual logs - testing never crashes out.

-- Coming back to this after writing a bunch of parsers in Applicative,
-- this looks like a great use case. Since we haven't learned them at
-- this point, I'll just leave this as is.
parseMessage :: String -> LogMessage
parseMessage s = case words s of
                   "E":level:time:msg -> LogMessage (Error (read level :: Int)) (read time :: Int) (unwords msg)
                   "I":time:msg -> LogMessage Info (read time :: Int) (unwords msg)
                   "W":time:msg -> LogMessage Warning (read time :: Int) (unwords msg)
                   text -> Unknown (unwords text)

-- parse a list of messages
parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)


-- Exercise 2: Insert Nodes into a MessageTree, sorting by timestamp (ts).
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert l Leaf = Node Leaf l Leaf
insert msg@(LogMessage _ ts _) (Node left root@(LogMessage _ rootts _) right)
       | ts < rootts = Node (insert msg left) root right
       | ts >= rootts = Node left root (insert msg right)
insert _ tree = tree

-- Exercise 3: Build a MessageTree by folding over a list of LogMessages.
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4: Grab an ordered list of LogMessages by going over the tree.
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left root right) = inOrder left ++ [root] ++ inOrder right

-- Exercise 5
isGraveError :: LogMessage -> Bool
isGraveError (LogMessage (Error level) _ _) = level >= 50
isGraveError _ = False

getMessageFromLog :: LogMessage -> String
getMessageFromLog (LogMessage _ ts msg) = show ts ++ ": " ++ msg
getMessageFromLog (Unknown _ ) = ""

filterOrderedLogs :: (LogMessage -> Bool) -> [LogMessage] -> [String]
filterOrderedLogs f = map getMessageFromLog . filter f . inOrder . build

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = filterOrderedLogs isGraveError

-- Exercise 6
betweenTimes :: Int -> Int -> LogMessage -> Bool
betweenTimes low high (LogMessage _ ts _) = low <= ts && ts < high
betweenTimes _ _ _ = False

getMustardLogs :: [LogMessage] -> [String]
getMustardLogs = filterOrderedLogs (betweenTimes 121 142)

-- By looking at logs near where the first mustard error occurred
-- eventually I found the name "Cory". Grepping for it in "error.log"
-- shows that his name pops up in many places. He could be our hacker.
