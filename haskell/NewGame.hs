{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-} -- Warnings enabled

module NewGame where

import Prelude hiding (pred, init)

import Data
import Data.List hiding (init)
import Control.Monad.Writer
import Control.Monad.State

type Output = String

type O a = Writer Output a

data St = St
    { types  :: [Type]
    , preds  :: [Pred]
    , consts :: [Constructor]
    , games  :: [Game]
    , init :: Maybe Initial
    }

type M a = State St a

runGame :: M () -> FilePath -> IO ()
runGame g fp =
    let (_, ceptreOutput) = runWriter (createGameFromSt st)
        (_, st) = runState g initSt
        initSt = St
            { types  = []
            , preds  = []
            , consts = []
            , games  = []
            , init   = Nothing
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

--TODO
applyVar :: Constructor -> Var -> Var
applyVar = undefined




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
    createTypes  (reverse $ types  st)
    createPreds  (reverse $ preds  st)
    createConsts (reverse $ consts st)
    createGames  (reverse $ games  st)
    --case init st of
    --    Nothing -> error "You must have a initial context"
    --    Just i -> createInit i


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

--TODO Test
createGames :: [Game] -> O ()
createGames = mapM_ createGame
    where
        createGame :: Game -> O ()
        createGame = \case
            Stage n impls isInteractive -> do
                tell' $ "stage " ++ n ++ " = {"
                mapM_ (\(impl , ident) -> do
                            tell' ident
                            createImplication impl
                      )
                      (zip impls
                           (map (\i -> n ++ '/' : show i) ([1..] :: [Integer]))
                      )
                tell' "}"
                when isInteractive $ tell' $ "#interactive " ++ n ++ "."
            Transition n impl -> do
                tell' n
                createImplication impl

        createImplication :: Implication -> O ()
        createImplication (Implication ls rs) = do
                tell' ": "
                ls' <- mapM createAppliedPred ls
                tell $ intercalate "\n\t* " ls'
                tell "\t-o "
                rs' <- mapM createAppliedPred rs
                tell $ intercalate "\n\t* " rs'
                tell "."

-- Create the ceptre string from a Pred
--TODO Test
createAppliedPred :: Pred -> O String
createAppliedPred = let
    checkVars:: Name -> [Type] -> [Var] -> O String
    checkVars n ts vars = do
        when (length ts /= length vars) $ error "Wrong number of vars applied to a pred"
        apreds <- zipWithM checkVar ts vars
        return $ n ++ ' ':(intercalate " " apreds)

    checkVar :: Type -> Var -> O String
    checkVar t = \case
        Pattern n tp ->
            if (t /= tp)
            then error "Wrong type when applying"
            else return n
        AVar (Constructor n ts tc) vars -> do
            when (t /= tc) $ error "Wrong type when applying"
            checkedvars <- zipWithM checkVar ts vars
            let checkedvars' = intercalate " " checkedvars
            return $ n ++ " (" ++ checkedvars' ++ ")"

    in \case
    Pred n ts -> if ts == [] then return n
        else error "Predicate needs to be applied to vars"
    Bwd n ts  -> if ts == [] then return n
        else error "bwd needs to be applied to vars"
    StagePred n -> return $ "stage " ++ n
    ApplyPred pred vars -> case pred of
        Pred n ts -> checkVars n ts vars
        Bwd  n ts -> checkVars n ts vars
        StagePred _-> error "Can't apply something to a Stage predicate"
        -- TODO Can this be supported? Does it make sense?
        ApplyPred _ _-> error "Applying something to an already applied thing isn't supported"


--TODO Test
createInit :: Initial -> O ()
createInit (Initial n ps) = do
    tell "#trace _ "
    tell n
    tell "\n\t{ "
    appliedPreds <- mapM createAppliedPred ps
    tell $ intercalate "\n\t, " appliedPreds
    tell "}."
