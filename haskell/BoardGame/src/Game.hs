module Game where

import Data.List.Split
import Data.List
import Control.Monad.Writer


-- Datatype for creating single predicates
data Game = Spred [String]
          | Type [String]
          | Tpred String [String]
          | Move [String]
          | Rule [String]
          | StageInteractive String Game
          | Stage String Game
          | Turn [String]
          | Trace
          | And Game Game
          deriving Show



type Output = String

type M a = Writer Output a

runGame :: Game -> FilePath -> IO ()
runGame g fp = do
    let (res, ceptreOutput) = runWriter (createGame g)
    writeFile fp (ceptreOutput)


-- createGames :: [Game] -> M ()
-- createGames games = foldM (\_ g -> createGame g) () games

-- Write any string with \n function to the file
-- write :: Game -> FilePath -> IO ()
-- write pr fp = do
--     appendFile fp (createGame pr)

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
    (Type (x:xs))      -> tell (x ++ " : type.\n")
                          >>= \_ -> createGame (Type xs)
    (StageInteractive str game) -> tell $ "stage " ++ str ++ " = {\n"
                                ++ createString game ++ "}\n#interactive game.\n\n"
    (Stage str game)   -> tell $ transition ++ "stage " ++ str ++ " = {\n" ++ createRules game ++ "}\n\n"
    (Turn xs)          -> tell $ createTurn xs
    Trace              -> tell $ "#trace _ game init."
    And g1 g2 -> do
        createGame g1
        createGame g2

-- The transition method for going into the stage result.
transition :: String
transition = "game_to_res :\n\tqui * stage game * token A * token B -o stage result.\n\n"

-- Simply create the string for simultanious turns
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
createString :: Game -> String
createString (Move [])             = ""
createString (Move (x:xs)) = "\tpick_" ++ x ++ "\n\t\t: turn A -o "
                              ++ x ++ " A * token A.\n" ++ (createString (Move xs))
