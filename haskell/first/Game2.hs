module Game2 where


-- Datatype for creating single predicates
type Name = String

data Game = Spred [String]
          | Tpred String [String]
          | Move String
          | Stage Name [Game]

          deriving Show


-- Write any string with \n function to the file

clearFile :: FilePath -> IO ()
clearFile fp = writeFile fp ""

write :: Game -> FilePath -> IO ()
write pr fp = do
    appendFile fp (createGame pr ++ "\n")


-- Creates single predicates
predicates :: [String] -> Game
predicates xs = Spred xs


-- Creates predicates with a type
prodicates :: String -> [String] -> Game
prodicates t preds = Tpred t preds


stage :: Name -> [Game] -> Game
stage name games = Stage name games

moves = map move

move :: String -> Game
move = Move

-- Creates the string that are able to write to the file
createGame :: Game -> String
createGame (Spred [])     = ""
createGame (Spred (x:xs)) = x ++ " : pred.\n" ++ createGame (Spred xs)
createGame (Tpred t [])     = ""
createGame (Tpred t (x:xs)) = x ++ " " ++ t ++ " : pred.\n" ++ createGame (Tpred t xs)
createGame (Move s)       =
        "pick_" ++ s ++ "\n\t: turn A -o " ++ s ++ " A * token A."
createGame (Stage name games) =  "stage " ++ name ++ " = {\n"
                         ++ unlines (map ("\t" ++) ((lines.unlines) (map createGame games)))
                         ++ "}\n"
                         ++ "#interactive game."
