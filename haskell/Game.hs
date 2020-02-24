module Game where

import Data.List.Split
import Data.List
import Control.Monad.Writer

{- TODO maybe
    * Implicitly create predicates according to the moves/rules
        e.g this shouldn't be n
    * Ordering of the rules shouldn't matter (Ceptre is order sensitive)
    * Auto-add predicates that are always present, maybe "token", "player"

-}
-- Datatype for creating single predicates
data Game = Spred [String]
          | Type [String]
          | Tpred String [String]
          | Move [String]
          | Rule [String]
          | StageInteractive String Game
          | Stage String Game
          | Turn [String]
          | Trace String
          | And Game Game
          deriving Show



type Output = String

type M a = Writer Output a

runGame :: Game -> FilePath -> IO ()
runGame g fp = do
    let (res, ceptreOutput) = runWriter (createGame g)
    writeFile fp (ceptreOutput)



--Combinator
(&) :: Game -> Game -> Game
g1 & g2 = g1 `And` g2


-- Creates single predicates
predicates :: [String] -> Game
predicates xs = Spred xs

--Create types
createType :: [String] -> Game
createType xs = Type xs

-- Creates predicates with a type
prodicates :: String -> [String] -> Game
prodicates t preds = Tpred t preds

-- Create moves into a StageInteractive
moves :: String -> [String] -> Game
moves name xs = StageInteractive name $ Move xs

-- Create rules for win condition
winCondition :: [String] -> Game
winCondition xs = Stage "result" $ Rule xs

-- Create rules turnbased system
generateTurn :: [String] -> Game
generateTurn xs = Turn xs

trace :: String -> Game
trace = Trace



-- Creates the string that are able to write to the file
createGame :: Game -> M ()
createGame g =  case g of
    (Spred [])         -> tell "\n"
    (Spred (x:xs))     -> do tell $ x ++ " : pred.\n"
                             createGame (Spred xs)
    (Tpred t [])       -> tell $ "\n"
    (Tpred t (x:xs))   -> do tell $ x ++ " " ++ t ++ " : pred.\n"
                             createGame (Tpred t xs)
    (Type [])          -> tell $ "\n"
    (Type (x:xs))      -> do tell (x ++ " : type.\n")
                             createGame (Type xs)
    (StageInteractive str game) -> tell $ "stage " ++ str ++ " = {\n"
                                ++ createString game ++ "}\n#interactive game.\n\n"
    (Stage str game)   -> tell $ transition ++ "stage " ++ str ++ " = {\n" ++ createRules game ++ "}\n\n"
    (Turn xs)          -> tell $ createTurn xs
    Trace name         -> tell $ "#trace _ " ++ name ++" init."
    And g1 g2 -> do
        createGame g1
        createGame g2

-- The transition method for going into the stage result.
-- TODO this function assumes that we have a token predicate
transition :: String
transition = "game_to_res :\n\tqui * stage game * token A * token B -o stage result.\n\n"

-- Simply create the string for simultanious turns
-- TODO this function assumes that we have a player predicate
createTurn :: [String] -> String
createTurn xs = head xs ++ " : player.\n" ++ last xs ++ " : player.\n\n"
    ++ "context init = \n" ++ "{turn " ++ head xs ++ ", turn " ++ last xs ++ "}.\n\n"

-- Helper function for creating the rules
createRules :: Game -> String
createRules (Rule [])   = ""
createRules (Rule (x:xs)) = case (word !! 1) -- takes middle word and use it for cases
    of "beats" -> winString  (head word) (last word) ++ "\n" ++ createRules (Rule xs)
       "draws" -> drawString (head word) (last word) ++ "\n" ++ createRules (Rule xs)
       where word = splitOn " " x

-- Helper function for creating the wincondition string
winString :: String -> String -> String
winString s1 s2 = "\twin_" ++ s1 ++ "\n\t\t: " ++ s1 ++ " A * " ++ s2 ++ " B -o win A * lose B."

-- Helper function for creating the drawcondition string
drawString :: String -> String -> String
drawString s1 s2 = "\tdraw_" ++ s1 ++ "\n\t\t: " ++ s1 ++ " A * " ++ s2 ++ " B -o turn A * turn B."

-- Helper function for createGame
-- TODO this function assumes that we have a token predicate
createString :: Game -> String
createString (Move [])             = ""
createString (Move (x:xs)) = "\tpick_" ++ x ++ "\n\t\t: turn A -o "
                              ++ x ++ " A * token A.\n" ++ (createString (Move xs))
