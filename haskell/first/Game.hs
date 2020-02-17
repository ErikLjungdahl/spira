module Game where


-- Datatype for creating single predicates
data Game = Spred [String] 
          | Tpred String [String] 
          | Move [String]
          | Stage String Game
          deriving Show


-- Write any string with \n function to the file
write :: Game -> FilePath -> IO ()
write pr fp = do
    appendFile fp (createGame pr)


-- Creates single predicates
predicates :: [String] -> Game
predicates xs = Spred xs


-- Creates predicates with a type
prodicates :: String -> [String] -> Game
prodicates t preds = Tpred t preds


-- Create moves into a Stage
moves :: String -> [String] -> Game
moves name xs = Stage name $ Move xs


-- Creates the string that are able to write to the file
createGame :: Game -> String
createGame (Spred [])         = "\n"
createGame (Spred (x:xs))     = x ++ " : pred.\n" ++ createGame (Spred xs)
createGame (Tpred t [])       = "\n"
createGame (Tpred t (x:xs))   = x ++ " " ++ t ++ " : pred.\n" ++ createGame (Tpred t xs)
createGame (Stage str game)   = "stage " ++ str ++ " = {\n" ++ createString game ++ "}\n#interactive game.\n"


-- Helper function for createGame 
createString :: Game -> String
createString (Move [])             = ""
createString (Move (x:xs)) = "pick_" ++ x ++ "\n\t: turn A -o " 
                              ++ x ++ " A * token A.\n" ++ (createString (Move xs))
