module Game where
import Data.List.Split
import Control.Monad.Writer

-- Datatype for creating single predicates
data Game = Pred String [String] 
          | Type [String]
          | Move [String]
          | Rule [String]
          | StageInteractive String Game
          | Stage String Game
          | Turn [String]
          | Trace 
          deriving Show

-- Write any string with \n function to the file
-- write :: Game -> FilePath -> IO ()
-- write pr fp = do
--    appendFile fp (createGame pr)

createType :: [String] -> Writer [String] ()
createType xs = tell $ [typeString xs]





-- Creates single predicates
predicates :: String -> [String] -> Game
predicates str xs = Pred str xs

--Create typesh
createType :: [String] -> Game
createType xs = Type xs

-- Create moves into a StageInteractive
moves :: String -> [String] -> Game
moves name xs = StageInteractive name $ Move xs

-- Create rules for win condition
winCondition :: [String] -> Game
winCondition xs = Stage "result" $ Rule xs

-- Create rules turnbased system
generateTurn :: [String] -> Game
generateTurn xs = Turn xs

createGame :: Game -> Writer [String] ()
createGame (Pred t xs)                 = tell $ [predString t xs]
createGame (Type xs)                   = tell $ [typeString xs]
createGame (StageInteractive str game) = tell $ [stageInString str game]
createGame (Stage str game)            = tell $ [stageString str game]
createGame (Turn xs)                   = tell $ [createTurn xs]
createGame (Trace)                     = tell $ ["#trace _ game init."]


doStuff :: Game -> Writer [String] Double
doStuff g = do
  tell "Hello"
  tell "NextHello"
  tell "NextHello2"
  return g

data MyMonad a = Logger (a,[String])

intane Monad MyMonad where
  return a = L (a,[])
  (Logger (a,log)) >>= f = let (Logger (b,log2)) = f a
                           in (Logger (b,log ++ log2))

predString :: String -> [String] -> String
predString t []     = "\n"
predString t (x:xs) = x ++ " " ++ t ++ " : pred.\n" ++ predString t xs

typeString :: [String] -> String
typeString []     = "\n"
typeString (x:xs) = x ++ " : type.\n" ++ typeString xs

stageInString :: String -> Game -> String
stageInString str game = "stage " ++ str ++ " = {\n" 
    ++ createString game ++ "}\n#interactive game.\n\n"

-- Helper function for createGame 
createString :: Game -> String
createString (Move [])             = ""
createString (Move (x:xs)) = "\tpick_" ++ x ++ "\n\t\t: turn A -o " 
                              ++ x ++ " A * token A.\n" ++ (createString (Move xs))

stageString :: String -> Game -> String
stageString str game = transition ++ "stage " ++ str ++ " = {\n" ++ createRules game ++ "}\n\n"

-- Simply create the string for simultanious turns
createTurn :: [String] -> String
createTurn xs = head xs ++ " : player.\n" ++ last xs ++ " : player.\n\n" 
    ++ "context init = \n" ++ "{turn " ++ head xs ++ ", turn " ++ last xs ++ "}.\n\n"

-- The transition method for going into the stage result.
transition :: String
transition = "game_to_res :\n\tqui * stage game * token A * token B -o stage result.\n\n"

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


















