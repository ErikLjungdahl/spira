module NewGame where

import Data

import Data.List.Split
import Data.List
import Control.Monad.Writer
import Control.Monad.State

type Output = String

type MO a = Writer Output a

data St = St
    { preds :: [Pred]
    , game :: [Game]
    , inits' :: Initial
    }

type M a = State St a

runGame :: M () -> FilePath -> IO ()
runGame g fp = undefined
--    let (res, ceptreOutput) = runWriter (createGame (preds' state & game state))
--        (_, state) = runState g initSt
--        initSt = St
--            { preds' = Nop
--            , game = Nop
--            , inits' = Nop
--            }
--    in writeFile fp (ceptreOutput)

newPred :: Type -> String -> M Pred
newPred t s = undefined
    --do
    --let p = predicates [t] [s]
    --addPred p
    --return p

newType :: String -> M Type
newType t = undefined
    -- do
    -- let p = type' t
    -- addPred p
    -- return p


addGame :: Game -> M ()
addGame g = do
    modify (\st -> st { game = g : game st })

addPred :: Pred -> M ()
addPred g = do
    modify (\st -> st { preds = g : preds st})
