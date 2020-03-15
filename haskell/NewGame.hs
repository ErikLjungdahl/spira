{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-} -- Warnings enabled

module NewGame where

import Prelude hiding (pred, init)

import Data
import Data.List hiding (init)
import Control.Monad.Writer
import Control.Monad.State

type Output = String

-- TODO Add Error monad (Except)
type O a = Writer Output a

data St = St
    { types  :: [Type]
    , preds  :: [Pred]
    , consts :: [Constructor]
    , games  :: [Game]
    , init :: Maybe Initial
    , nbrOfPatterns :: Int
    , player :: Type
    , nats :: (Type,Constructor,Constructor)
    , turn :: Pred
    , drawStage :: (Pred,Pred,Pred)
    }

type M a = State St a

runGame :: M () -> FilePath -> IO ()
runGame g fp =
    let (_, ceptreOutput) = runWriter (createGameFromSt st)
        (_, st) = runState (default_config >>=(\_ -> g)) initSt
        initSt = St
            { types  = []
            , preds  = []
            , consts = []
            , games  = []
            , init   = Nothing
            , nbrOfPatterns = 0
            }
        default_config :: M ()
        default_config = do
            player <- newType "player"
            -- turn_p <- newPredWithType "turn" [player]

            modify (\st -> st {player = player})
            initNats

            -- Draw, goes here in case of no available choice
            stageDraw

    in writeFile fp ceptreOutput

stageDraw :: M ()
stageDraw = do
    player <- gets player
    drawPred <- newPredWithType "draw" [player]
    varPlayer <- newVar player
    end <- newPred "end"

    let appliedDraw = applyPred drawPred [varPlayer]
    drawStage <- stage "draw" False [[appliedDraw] -* [end]] varPlayer
    modify (\st -> st {drawStage = drawStage})

newPred :: String -> M Pred
newPred s = do
    let p = Pred s []
    addPred p
    return p

newBwd :: String -> [Type] -> M Pred
newBwd s tx = do
    let p = Bwd s tx
    addPred p
    return p

--TODO check that Pred doesn't already exist
newPredWithType :: Name -> [Type] -> M Pred
newPredWithType s xt = do
    let p = Pred s xt
    addPred p
    return p


--TODO check that Constructor doesn't already exist
newConstructor :: Name -> [Type] -> Type -> M Constructor
newConstructor s xt t = do
    let c = Constructor s xt t
    modify (\st -> st { consts = c : consts st})
    return c

--TODO check that type doesn't already exist
newType :: Name -> M Type
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


players :: [String] -> M Type
players names = do
    player <- gets player -- newType "player"
    opp <- newBwd "opp" [player, player]
    initiatePlayers names player
    --initiateOpponents names names opp
    return player
    where
        initiatePlayers [] p = return ()
        initiatePlayers (n:ns) p = do
            newConstructor n [] p
            initiatePlayers ns p

        --initiateOpponents []      n2  opp = return ()
        --initiateOpponents (n1:ns) (n2:n2s) opp = newPattern n1 n2 opp

        --opponentHelper you []      = return ()
        --opponentHelper you (n2:ns) = undefined



initNats :: M ()
initNats = do
    nat <- newType "nat"
    z <- newConstructor "z" [] nat
    s <- newConstructor "s" [nat] nat
    modify (\st -> st {nats=(nat,s,z)})


stage :: Name -> IsInteractive -> [Implication] -> Var -> M (Pred,Pred,Pred)
stage n isInteractive impls playerVar= do
    player <- gets player
    preToken <- newPredWithType ("pretoken_" ++ n)[player]
    posToken <- newPredWithType ("postoken_" ++ n)[player]

    let appliedPreToken = applyPred preToken [playerVar]
        appliedPosToken = applyPred posToken [playerVar]
        impls' = map (\(Implication l r) -> Implication
                                                (appliedPreToken : l)
                                                (appliedPosToken : r) )
                      impls
        s = Stage n impls' isInteractive
    addGame s
    let stagePred = StagePred n
    let res = (preToken, stagePred, posToken)

    -- Add draw
    when isInteractive $ do
        drawStage <- gets drawStage
        transition (n ++ "_to_draw")
                   ((res `toStageWith` playerVar)
                   -*
                    (drawStage `toStageWith` playerVar))

    return res

fromStageWith :: (Pred,Pred,Pred) -> Var -> [Pred]
fromStageWith (_,stagePred,posToken) v =
    [stagePred, applyPred posToken [v]]

toStageWith :: (Pred,Pred,Pred) -> Var -> [Pred]
toStageWith (preToken,stagePred,_) v =
    [applyPred preToken [v], stagePred]

fromStageToStageWith :: (Pred,Pred,Pred) -> (Pred,Pred,Pred) -> Var -> Implication
fromStageToStageWith from to p =
    (from `fromStageWith` p)
    -*
    (to `toStageWith` p)
fromFailedStageToStageWith :: (Pred,Pred,Pred) -> (Pred,Pred,Pred) -> Var -> Implication
fromFailedStageToStageWith from to p =
    (from `toStageWith` p)
    -*
    (to `toStageWith` p)

transition :: Name -> Implication -> M ()
transition n (Implication ls rs) = do
    let impl = Implication (qui : ls) rs
    addGame (Transition n impl)
qui :: Pred
qui = Pred "qui" []

applyVar :: Constructor -> [Var] -> Var
applyVar c vs = AVar c vs

-- Applies a constructor to a Var n times,
-- useful for recursive constructors such as suc
applyVarTimes :: Constructor -> Var -> Int -> Var
applyVarTimes s x 0 = x
applyVarTimes s x i = applyVar s [(applyVarTimes s x (i-1))]


newVar :: Type -> M Var
newVar t = do
    n <- gets nbrOfPatterns
    let l = (['A'..] !! n) :[]
    modify (\st -> st {nbrOfPatterns = n + 1})
    return $ Pattern l t

applyPred :: Pred -> [Var] -> Pred
applyPred p vars = ApplyPred p vars

--TODO
addAppliedPredToInit :: Pred -> M ()
addAppliedPredToInit = undefined



(-*) :: [Pred] -> [Pred] -> Implication
ps1 -* ps2 = Implication ps1 ps2

-- Pred has to have the constructor "Pred _ [player, nat nat]"
--inARow :: Board -> Int -> Pred -> Implication
inARow :: Int -> Pred -> Var -> M Implication
inARow n pred playerVar = do
    player <- gets player
    (nat, s, z) <- gets nats

    x <- newVar nat
    y <- newVar nat

    let occupiedAs = map (\i -> applyPred pred [playerVar, applyVarTimes s x i, y]) [0..n-1]

    return $ Implication occupiedAs []

--TODO
inAColumn :: Int -> Pred -> M Implication
inAColumn = undefined
inADiagonal :: Int -> Pred -> M Implication
inADiagonal = undefined


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
    tell' "% TYPES"
    createTypes  (reverse $ types  st)
    tell' "\n% PREDS AND BWDS"
    createPreds  (reverse $ preds  st)
    tell' "\n% CONSTRUCTORS"
    createConsts (reverse $ consts st)
    tell' "\n% STAGES AND TRANSITIONS"
    createGames  (reverse $ games  st)
    tell' "\n% INITIAL"
    case init st of
        Nothing -> return () --TODO give error instead -- error "You must have a initial context"
        Just i -> createInit i


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
            ApplyPred p vars -> case p of
                Bwd n ts -> do
                    appliedPred <- checkVars n ts vars
                    tell' $ appliedPred ++ "."
                _ -> error "You can't apply a predicate to a variable in the top level"


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
                tell' "\n}"
                when isInteractive $ tell' $ "#interactive " ++ n ++ "."
                tell' ""
            Transition n impl -> do
                tell' n
                createImplication impl
                tell'' ""

        createImplication :: Implication -> O ()
        createImplication (Implication ls rs) = do
                tell "\t: "
                ls' <- mapM createAppliedPred ls
                tell' $ intercalate "\n\t* " ls'
                tell "\t-o "
                rs' <- mapM createAppliedPred rs
                tell $ intercalate "\n\t* " rs'
                tell "."

-- Create the ceptre string from a Pred
--TODO Test
checkVars:: Name -> [Type] -> [Var] -> O String
checkVars n ts vars = let
    checkVar :: Type -> Var -> O String
    checkVar t = \case
        Pattern n tp ->
            if (t /= tp)
            then error "Wrong type when applying"
            else return n
        AVar (Constructor n ts tc) vars -> do
            when (t /= tc) $ error "Wrong type when applying"
            checkedvars <- zipWithM checkVar ts vars
            if checkedvars == []
            then return n
            else let checkedvars' = intercalate " " checkedvars
                 in return $ "(" ++ n ++ " " ++ checkedvars' ++ ")"
    in do
    when (length ts /= length vars) $ error "Wrong number of vars applied to a pred"
    apreds <- zipWithM checkVar ts vars
    return $ n ++ ' ':(intercalate " " apreds)


createAppliedPred :: Pred -> O String
createAppliedPred = \case
    Pred n ts -> if ts == [] then return n
        else error $ "Predicate "++n++" needs to be applied to vars"
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
