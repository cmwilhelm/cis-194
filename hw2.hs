module LogAnalysis where

import Log

strToInt :: String -> Int
strToInt t = read t :: Int

makeInfo :: [String] -> LogMessage
makeInfo (_:t:msg) = LogMessage Info (strToInt t) (unwords msg)

makeWarning :: [String] -> LogMessage
makeWarning (_:t:msg) = LogMessage Warning (strToInt t) (unwords msg)

makeError :: [String] -> LogMessage
makeError (_:c:t:msg) = LogMessage (Error code) (strToInt t) (unwords msg)
  where code = strToInt c

makeUnknown :: [String] -> LogMessage
makeUnknown msg = Unknown (unwords msg)

chooseConstructor :: [String] -> ([String] -> LogMessage)
chooseConstructor [] = makeUnknown
chooseConstructor (t:xs)
  | t == "I" = makeInfo
  | t == "W" = makeWarning
  | t == "E" = if xs == [] then makeUnknown else makeError

parseMessage :: String -> LogMessage
parseMessage line = constructor lineWords
  where constructor = chooseConstructor lineWords
        lineWords   = words line

parse :: String -> [LogMessage]
parse fileContents = map parseMessage (lines fileContents)

compareTimes :: LogMessage -> LogMessage -> Ordering
compareTimes (LogMessage _ t1 _) (LogMessage _ t2 _) =  compare t1 t2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf         = Node Leaf msg Leaf
insert msg (Node l nMsg r)
  | compareTimes msg nMsg == GT = Node (insert msg l) nMsg r
  | otherwise                   = Node l nMsg (insert msg r)

build :: [LogMessage] -> MessageTree
build msgs = foldl (flip insert) Leaf msgs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf           = []
inOrder (Node l msg r) = inOrder l ++ [msg] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = extractMessages $ inOrder $ build (filter isSevere msgs)
  where isSevere (LogMessage (Error severity) _ _) = severity >= 50
        isSevere _                                 = False
        extractMessages msgs'                      = map extractMessage msgs'
        extractMessage (LogMessage _ _ msg)        = msg


main :: IO ()
main = do
  messages <- whatWentWrong . parse <$> readFile "error.log"
  print messages
