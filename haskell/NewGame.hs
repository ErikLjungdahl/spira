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
    , types :: [Type]
    }

type M a = State St a

runGame :: M () -> FilePath -> IO ()
runGame g fp = 
    --let (res, ceptreOutput) = runWriter (createGame (preds' state & game state))
    let 
        (_, state) = runState g initSt
        initSt = St
            { preds = []
            , game = []
            --, inits' = 
            , types = []
            }
    in writeFile fp (show (preds state) ++ show (types state) ++ show (game state) )

newPred :: String -> M Pred
newPred s = undefined 
    --let p = predicates [t] [s]
    --addPred p
    --return p

newPredWithType :: String -> [Type] -> Type -> M Pred
newPredWithType s xt t = do
    let npwt = Pred s xt t
    addPred npwt
    return npwt


newType :: String -> M Type
newType t = do
    let ty = Type t
    addType ty
    return ty
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

addType :: Type -> M ()
addType g = do
    modify (\st -> st { types = g : types st})

nats :: M Type
nats = do
    nat <- newType "nat"
    pNat <- newPredWithType "z" [] nat
    ppNat <- newPredWithType "s" [nat] nat
    return nat





