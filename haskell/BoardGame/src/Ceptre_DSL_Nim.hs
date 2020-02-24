module Ceptre_DSL_Nim where

-- Datatype for creating single predicates
data Game = Player [String]
          | Pred [String]
          | CountSticks [String]
          | NbrOfSticks [String]
          | OppPlayer String String
          | Stage String Game
          | Move [String]
          -- | ContextInit [String]
            deriving Show

-- Write any string with \n function to the file
write :: Game -> FilePath -> IO ()
write pr fp = do
    appendFile fp  (createGame pr)

createCountSticks :: [String] -> Game
createCountSticks xs = CountSticks xs

createPlayer :: [String] -> Game
createPlayer x = Player x

createNbrOfSticks :: [String] -> Game
createNbrOfSticks x = NbrOfSticks x

createOppPlayer :: String -> String -> Game
createOppPlayer x y = OppPlayer x y 

createStage :: String -> [String] -> Game
createStage x xs = Stage x (Move xs)

--createContextInit :: [String] -> Game
--createContextInit xs = ContextInit xs

-- Creates the string that are able to write to the file
createGame :: Game -> String 
createGame (CountSticks []) = ""
createGame (CountSticks xs) = "nat : type.\n" ++ helpSticks xs

createGame (NbrOfSticks []) = ""
createGame (NbrOfSticks x) = "\n" ++ helpNbrOfSticks x ++ " : pred.\n"

createGame (Player []) = ""
createGame (Player xs) = "\nplayer : type.\n" ++ helpPlayer xs

createGame (OppPlayer x y) = "\nopp player player : bwd.\nopp " ++ x ++ " " 
    ++ y ++ "." ++ "\nopp " ++ y ++ " " ++ x ++ "." ++ "\n\n"

createGame (Pred []) = ""
createGame (Pred (x:xs)) = x ++ " player : pred.\n" ++ createGame (Pred xs)

createGame (Stage x (Move xs)) = "stage " ++ x ++ "{\n" ++ helpCreateStage xs ++ "}\n#interactive " ++ x ++ "."

--createGame (ContextInit []) = ""
--createGame (ContextInit xs) = "context init =\n{ " ++ xs ++ successor x s ++ "\n\n #trace _ rules init."

helpCreateStage :: [String] -> String
helpCreateStage [] = ""
helpCreateStage (x:xs) = "\t" ++ x ++ "\t\t: turn P * opp P P'\n\t\t\t\t* sticks " ++ helpToDigits x ++ "\n\t\t\t-o sticks N * turn P'.\n\n" ++ helpCreateStage xs

helpToDigits :: String -> String
helpToDigits (x) 
    | x == "takeOne"   = "(s N)"
    | x == "takeTwo"   = "(s (s N))"
    | x == "takeThree" = "(s (s (s N)))"

-- Helper function for counting sticks
helpSticks :: [String] -> String
helpSticks [] = ""
helpSticks (x:xs) = x ++ " : nat.\n" ++ helpSticks xs 

-- Helper function for showing the number of sticks
helpNbrOfSticks :: [String] -> String
helpNbrOfSticks [] = ""
helpNbrOfSticks (x:xs) = x ++ helpNbrOfSticks xs 

-- Helper function for Player
helpPlayer :: [String] -> String
helpPlayer [] = ""
helpPlayer (x:xs) = x ++ " : player.\n" ++ helpPlayer xs

-- Creates single predicates
predicate :: [String] -> Game
predicate xs = Pred xs

successor :: Int -> String -> String
successor x s = suc x s
  where
    suc 0 s = s
    suc x s = "(s " ++ suc (x-1) s ++ ")"
