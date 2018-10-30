> {-# OPTIONS_GHC -Wall #-}
> module LogAnalysis where
>
> import Log

First parse an individual Message:

> -- | @parseMessage message@ creates a @LogMessage@ from a string
> parseMessage :: String -> LogMessage
> parseMessage message = messageFromWords (words message)

> -- | @messageFromWords words@ creates a @LogMessage@ from the words in a log line 
> messageFromWords :: [String] -> LogMessage
> messageFromWords ("I":ts:str)       = LogMessage Info (read ts :: Int) (unwords str)
> messageFromWords ("W":ts:str)       = LogMessage Warning (read ts :: Int) (unwords str)
> messageFromWords ("E":level:ts:str) = LogMessage (Error (read level :: Int)) (read ts :: Int) (unwords str)
> messageFromWords str                = Unknown (unwords str)

> -- 
> parse :: String -> [LogMessage]
> parse = map parseMessage . lines

Exercise 2
Sort the Log messages in timestamp order

> -- inserts a log message into a (Pre-sorted) MessageTree
> insert :: LogMessage -> MessageTree -> MessageTree
> insert (Unknown _) tree        = tree
> insert lm Leaf            = Node Leaf lm Leaf
> insert lm@(LogMessage _ ts _) (Node left middle@(LogMessage _ mts _ ) right)
>   | ts < mts = Node (insert lm left) middle right
>   | otherwise = Node left middle (insert lm right)

Exercise 3:
Build a tree

> -- | builds a message tree from a list of log messages
> build :: [LogMessage] -> MessageTree
> build [] = Leaf
> build (x:xs) = insert x (build xs)

Exercise 4:

> -- | inOrder traverses a MessageTree to make a list of ordered LogMessages
> inOrder :: MessageTree -> [LogMessage]
> inOrder Leaf = []
> inOrder (Node l m r) = (inOrder l) ++ [m] ++ (inOrder r)

Exercise 5:

> whatWentWrong :: [LogMessage] -> [String]
> whatWentWrong = extractSevereErrorMessages . inOrder . build

> -- | extractSevereErrorMessages takes a sorted list of LogMessages and returns a list of messages
> --   corresponding to any errors of severity 50 or more  
> extractSevereErrorMessages :: [LogMessage] -> [String]
> extractSevereErrorMessages [] = []
> extractSevereErrorMessages ((LogMessage (Error level) _ message):xs)
>    | level >= 50 = message:(extractSevereErrorMessages xs)
>    | otherwise   = extractSevereErrorMessages xs
> extractSevereErrorMessages (_:xs)  = extractSevereErrorMessages xs

Exercise 6:

> -- | printHacker prints name of hacker
> printHacker :: FilePath -> IO String
> printHacker file
>   = concat . map (take 1) . whatWentWrong . parse <$> readFile file
