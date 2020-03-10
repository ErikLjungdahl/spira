{-# LANGUAGE LambdaCase #-}
module NewGame where

import Data

import Data.List.Split
import Data.List
import Control.Monad.Writer
import Control.Monad.State

type Output = String

type O a = Writer Output a

data St = St
    { preds :: [Pred]
    , constructor :: [Constructor]
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

newConstructor :: Name -> [Type] -> Type -> M Constructor
newConstructor s xt t = do
    let c = Constructor s xt t
    modify (\st -> st { constructor = c : constructor st})
    return c


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
    pNat <- newConstructor "z" [] nat
    ppNat <- newConstructor "s" [nat] nat
    return nat






---- BACKEND ----
-- From our State (St) to ceptre

tell' :: String -> O ()
tell'  = \s -> tell  (s ++ "\n")
tell'' :: String -> O ()
tell'' = \s -> tell' (s ++ "\n")

createGameFromSt :: St -> O ()
createGameFromSt st = do
    createTypes (reverse $ types st)
    createPreds (reverse $ preds st)


createTypes :: [Type] -> O ()
createTypes ts = mapM_ (\(Type name) -> tell $ name ++ " : type.") ts

createPreds :: [Pred] -> O ()
createPreds = mapM_ createPred
    where
        createPred :: Pred -> O ()
        createPred = \case
            Pred name ts ->
                let ts' = map (\(Type a) -> a) ts
                in tell' $ name ++ ' ':(intercalate " " ts') ++ " : pred."


-- createGame :: Game -> O ()
-- createGame g =
--     let tell'  = \s -> tell  (s ++ "\n")
--         tell'' = \s -> tell' (s ++ "\n")
--    in case g of
--    Pred as [] t       -> tell $ "\n"
--    Pred as (x:xs) (Type t)    -> let as' = map (\(Type a) -> a) as
--        in do
--            tell' $ x ++ " " ++ (intercalate " " as') ++ " : " ++ t ++ "."
--            createGame (Pred as xs (Type t))
--    StageInteractive str preds -> tell'' $ "stage " ++ str ++ " = {\n"
--                                      ++ concatMap createString preds ++ "}\n#interactive game."
--    Stage str rules   -> do
--        tell' $ transition ++ "stage " ++ str ++ " = {"
--        mapM createGame rules -- TODO fix?
--        --tell' (intercalate "\n" rules')
--        tell "\n}"
--    Turn xs          -> tell' $ createTurn xs
--    Trace name         -> tell' $ "#trace _ " ++ name ++" init."
--    And g1 g2 -> do
--        createGame g1
--        createGame g2
--    Nop -> return ()
--    BoardContext x y -> tell'' $ allFree x y
--    Win  (Pred tx (x:_) _) (Pred ty (y:_) _) -> tell' $ winString x y
--    Draw (Pred tx (x:_) _) (Pred ty (y:_) _) -> tell' $ drawString x y
--    Rule string -> tell' string
