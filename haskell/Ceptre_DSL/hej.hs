


data Game = Pred [String]
          | Type String


write :: Game -> FilePath -> IO ()
write game fp = do
    appendFile fp (createGame game)


createGame :: Game -> String
createGame (Pred [])     = ""
createGame (Pred (x:xs)) = x ++ " : pred.\n" ++ createGame (Pred xs)
createGame (Type str) = str ++ " : type."



