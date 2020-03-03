module Game where

import Data.List.Split
import Data.List
import Control.Monad.Writer
import Control.Monad.State

{- TODO maybe
    * Implicitly create predicates according to the moves/rules
        e.g this shouldn't be n
    * Ordering of the rules shouldn't matter (Ceptre is order sensitive)
    * Auto-add predicates that are always present, maybe "token", "player"

-}
-- Datatype for creating single predicates
data Game = Spred [String]
          | Type [String]
          | Tpred [String] [String] String
          | Move [String]
          | Rule [String]
          | StageInteractive String Game
          | Stage String [String]
          | Turn [String]
          | Trace String
          | And Game Game
          | BoardContext Int Int
          | Nop
          deriving Show



type Output = String

type MO a = Writer Output a

--data Pred = Pred String

data St = St
    { preds' :: Pred
    , game :: Game
    , inits' :: Game
    }

type Pred = Game

type M a = State St a

newPred :: String -> M Pred
newPred s = do
    let p = Spred [s]
    modify (\st -> st { preds' = preds' st & p})
    return p

add :: Game -> M ()
add g = do
    modify (\st -> st { game = game st & g})

numbers :: M ()
numbers = add $
      types ["nat"] -- Remove later?
    & typePredicates [] ["z"] "nat" -- Remove later?
    & typePredicates ["nat"] ["s"] "nat"

players :: Int -> M ()
players n = do
    add $ types ["player"]
    add $ preds ["draw"]
    add $ predicates ["player"] ["turn","token","win","lose"] -- Remove lose?
    add $ typePredicates ["player","player"] ["opp"] "bwd"
    add $ typePredicates [] ["alice","bob"] "player"
    -- TODO these predicates should be bwds
    --add $ predicates ["alice bob"] ["opp"]
    --add $ predicates ["bob alice"] ["opp"]
    
    --modify (\st -> st {init = "turn alice"

board :: Int -> Int -> M ()
board x y = do
    add $ predicates ["nat","nat"] ["free","restore"]
    add $ predicates ["player","nat","nat"] ["occupied"]
    add $ preds ["full","not_full_yet"]
    add $ BoardContext x y

runGame :: M () -> FilePath -> IO ()
runGame g fp =
    let (res, ceptreOutput) = runWriter (createGame (preds' state & game state))
        (_, state) = runState g initSt
        initSt = St
            { preds' = Nop
            , game = Nop
            , inits' = Nop
            }
    in writeFile fp (ceptreOutput)


end :: M ()
end = add $ trace "play"

--Combinator
(&) :: Game -> Game -> Game
g1 & g2 = g1 `And` g2

--Create types
types :: [String] -> Game
types xs = Type xs

preds :: [String] -> Game
preds = predicates []

-- Creates predicates with a type
predicates :: [String] -> [String] -> Game
predicates ts preds = Tpred ts preds "pred"

-- Creates predicates with a type
typePredicates :: [String] -> [String] -> String -> Game
typePredicates as preds t = Tpred as preds t

-- Create moves into a StageInteractive
moves :: String -> [String] -> Game
moves name xs = StageInteractive name $ Move xs

-- Create rules for win condition
winCondition :: String -> [String] -> Game
winCondition n xs = Stage n $ xs

-- Create rules turnbased system
generateTurn :: [String] -> Game
generateTurn xs = Turn xs

trace :: String -> Game
trace = Trace

beats :: String -> String -> String
beats a b = winString a b

draws :: String -> String -> String
draws a b = drawString a b


inARow :: Int -> String
inARow n = '\t' : intercalate "\n\t\t" combined
    where
    begin    = "win/row"
    token    = ": token B"
    occupied = ["* occupied A " ++ successor n' "X" ++ " Y" | n' <- [0..n-1]]
    end      = "-o win A."
    combined = begin : token : occupied ++ [end]

inAColumn :: Int -> String
inAColumn n = '\t' : intercalate "\n\t\t" combined
    where
    begin    = "win/column"
    token    = ": token B"
    occupied = ["* occupied A " ++ "X " ++ successor n' "Y"  | n' <- [0..n-1]]
    end      = "-o win A."
    combined = begin : token : occupied ++ [end]

inADiagonal :: Int -> String
inADiagonal n =  combined
    where
    begin    = "win/diagonal/"
    token    = ": token B"
    occupiedUp   = ["* occupied A "
                 ++ successor n' "X" ++ " " ++ successor n' "Y"
                 | n' <- [0..n-1]]
    occupiedDown = ["* occupied A "
                 ++ successor n' "X" ++ " " ++ successor (n-1-n') "Y"
                 | n' <- [0..n-1]]
    end      = "-o win A."
    up   = (:) '\t' $ intercalate "\n\t\t" $ (begin ++ "up")  : token : occupiedUp   ++ [end]
    down = (:) '\t' $ intercalate "\n\t\t" $ (begin ++ "down") : token : occupiedDown ++ [end]
    combined = up ++ "\n" ++ down

successor :: Int -> String -> String
successor x s = suc x s
  where
    suc 0 s = s
    suc x s = "(s " ++ suc (x-1) s ++ ")"

allFree :: Int -> Int -> String
allFree x' y' = "context all_free = {" ++ generate (x'-1) (y'-1) ++ "\n}"
  where
    genStr x y = "\n\tfree\t" ++ successor x "z" ++ "\t" ++ successor y "z"
    generate x y
      | x == 0 && y == 0 = genStr x y
      | y == 0           = genStr x y ++ generate (x-1) (x'-1)
      | otherwise        = genStr x y ++ generate (x) (y-1)






-- Creates the string that are able to write to the file
createGame :: Game -> MO ()
createGame g =
    let tell'  = \s -> tell  (s ++ "\n")
        tell'' = \s -> tell' (s ++ "\n")
    in case g of
    Tpred as [] t        -> tell $ "\n"
    Tpred as (x:xs) t    -> do tell' $ x ++ " " ++ (intercalate " " as) ++ " : " ++ t ++ "."
                               createGame (Tpred as xs t)
    Type []              -> tell $ "\n"
    Type (x:xs)          -> do tell' (x ++ " : type.")
                           createGame (Type xs)
    StageInteractive str game -> tell'' $ "stage " ++ str ++ " = {\n"
                                      ++ createString game ++ "}\n#interactive game."
    Stage str rules      -> tell'' $ transition ++ "stage " ++ str ++ " = {\n"
                              ++ (intercalate "\n" rules)
                              ++ "\n}"
    Turn xs              -> tell' $ createTurn xs
    Trace name           -> tell' $ "#trace _ " ++ name ++" init."
    And g1 g2 -> do
        createGame g1
        createGame g2
    Nop -> return ()
    BoardContext x y -> tell'' $ allFree x y

-- The transition method for going into the stage result.
-- TODO this function assumes that we have a token predicate
transition :: String
transition = "game_to_res :\n\tqui * stage game * token A * token B -o stage result.\n\n"

-- Simply create the string for simultanious turns
-- TODO this function assumes that we have a player predicate
createTurn :: [String] -> String
createTurn xs = head xs ++ " : player.\n" ++ last xs ++ " : player.\n\n"
    ++ "context init = \n" ++ "{turn " ++ head xs ++ ", turn " ++ last xs ++ "}.\n\n"


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
