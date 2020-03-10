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
    { types  :: [Type]
    , preds  :: [Pred]
    , consts :: [Constructor]
    , games  :: [Game]
    , inits' :: Initial
    }

type M a = State St a

runGame :: M () -> FilePath -> IO ()
runGame g fp =
    let (res, ceptreOutput) = runWriter (createGameFromSt state)
        (_, state) = runState g initSt
        initSt = St
            { preds = []
            , games = []
            --, inits' =
            , types = []
            , consts = []
            }
    in writeFile fp ceptreOutput

newPred :: String -> M Pred
newPred s = undefined
    --let p = predicates [t] [s]
    --addPred p
    --return p

newConstructor :: Name -> [Type] -> Type -> M Constructor
newConstructor s xt t = do
    let c = Constructor s xt t
    modify (\st -> st { consts = c : consts st})
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
    modify (\st -> st { games = g : games st })

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
tell_ :: String -> O ()
tell_ = \s -> tell (s ++ " ")

createGameFromSt :: St -> O ()
createGameFromSt st = do
    createTypes (reverse $ types st)
    createPreds (reverse $ preds st)
    createConsts (reverse $ consts st)


createTypes :: [Type] -> O ()
createTypes ts = mapM_ (\(Type name) -> tell' $ name ++ " : type.") ts



createPreds :: [Pred] -> O ()
createPreds = mapM_ createPred
    where
        createPred :: Pred -> O ()
        createPred = \case
            Pred name ts -> helper name ts "pred"
            Bwd name ts -> helper name ts "bwd"
            StagePred _ -> error "You can't initialize a StagePred, don't put it in the state"
            ApplyPred _ _ -> error "You can't apply a predicate to a variable in the top level"


helper :: Name -> [Type] -> Name -> O ()
helper name ts right =
    let ts' = map (\(Type a) -> a) ts
    in tell' $ name ++ ' ':(intercalate " " ts') ++ " : " ++ right ++"."

createConsts :: [Constructor] -> O ()
createConsts cs = mapM_ (\(Constructor n ts t) ->
                            helper n ts (show t))
                        cs

createGames :: [Game] -> O ()
createGames = mapM_ createGame
    where
        createGame :: Game -> O ()
        createGame = \case
            Stage n impls -> do
                tell' $ "stage " ++ n ++ " = {"
                mapM_ (uncurry createImplication)
                      (zip impls
                           (map (\i -> n ++ '/' : show i) [1..]) )
                tell' "}"
        createImplication :: Implication -> String -> O ()
        createImplication (Implication ls rs) ident =
            let
            implPred :: Pred -> O ()
            implPred = \case
                Pred n ts -> if ts == [] then tell n
                    else error "Predicate needs to be applied to vars"
                Bwd n ts  -> if ts == [] then tell n
                    else error "bwd needs to be applied to vars"
                StagePred n -> tell $ "stage " ++ n
                ApplyPred pred vars -> case pred of
                    Pred n ts -> applyPreds' n ts vars
                    Bwd  n ts -> applyPreds' n ts vars
                    StagePred _-> error "Can't apply something to a Stage predicate"
                    -- TODO Can this be supported? Does it make sense?
                    ApplyPred _ _-> error "Applying something to an already applied thing isn't supported"

            applyPreds' n ts vars = do
                when (length ts /= length vars) $ error "Wrong number of vars applied to a pred"
                tell n
                zipWithM checkVar ts vars
                return ()

            checkVar :: Type -> Var -> O ()
            checkVar t = \case
                Pattern n tp ->
                    if (t /= tp)
                    then error "Wrong type when applying"
                    else tell $ "(" ++ n ++ ")"
                AVar (Constructor n ts tc) vars -> do
                    when (t /= tc) $ error "Wrong type when applying"
                    tell "("
                    tell n
                    zipWithM checkVar ts vars
                    tell ")"
            in do
                tell' ident
                tell' ": "
                mapM_ implPred ls
                tell "\t-o "
                mapM_ implPred rs
                tell "."



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
