{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-} -- Warnings enabled

module Game where
--TODO Only export the functions that we want the user to be able to use.

import Prelude hiding (pred, init, lookup)

import Data
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Control.Monad.Writer
import Control.Monad.State
import Data.Map.Lazy (insert, lookup, Map, empty)

import Debug.Trace

type Output = String

-- TODO Add Error monad (Except)
type O a = Writer Output a

data St = St
    { types  :: [Type]
    , preds  :: [Pred]
    , consts :: [Constructor]
    , games  :: [Game]
    , initStage :: Maybe Name
    , initialPreds :: [Pred]
    , nbrOfBindings :: Int
    , player :: Type
    , nats :: (Type,Const,Var)
    , turn :: Pred
    , drawStage :: (Pred,Pred,Pred)
    , columnNames :: Map Pred [Name]
    , board :: Board
    }

type M a = State St a

type Const = [Var] -> Var

runGame :: M () -> FilePath -> IO ()
runGame g fp =
    let (_, ceptreOutput) = runWriter (createGameFromSt st)
        (_, st) = runState (default_config >>=(\_ -> g)) initSt
        initSt = St
            { types  = []
            , preds  = []
            , consts = []
            , games  = []
            , initStage = Nothing
            , initialPreds = []
            , nbrOfBindings = 0
            , columnNames = empty
            }
        default_config :: M ()
        default_config = do
            player <- newType "player"
            -- turn_p <- newPred "turn" [player]

            modify (\st -> st {player = player})
            initNats

            -- Draw, goes here in case of no available choice
            initDrawStage

    in writeFile fp ceptreOutput

-- Generates the stage for draws
-- A player automatically goes to this stage if it has no
--      available choice in an interactive stage
initDrawStage :: M ()
initDrawStage = do
    player <- gets player
    drawPred <- newPred "draw" [player]
    varPlayer <- newBinding player
    end <- newPred "end" []

    let appliedDraw = applyPred drawPred [varPlayer]
    drawStage <- stage "draw" False [[appliedDraw] -* [end]] varPlayer
    modify (\st -> st {drawStage = drawStage})

-- Creates a Fact Pred
newFactType :: String -> [Type] -> M Pred
newFactType s tx = do
    let p = Bwd s tx
    addPred p
    return p
-- Creates a Fact Pred
newFactTypeWithNames :: String -> [Type] -> [Name] -> M Pred
newFactTypeWithNames s tx names = do
    p <- newFactType s tx
    modify $ \st -> st {columnNames = insert p names $ columnNames st}
    return p

-- Creates a fact.
-- Pred should be Bwd
-- [Var] can contain Bindings
newFact :: Pred -> [Var] -> M ()
newFact b vars = do
    let appliedFact = applyPred b vars
    addPred appliedFact

--
-- Takes two ApplyPreds on Bwd-predicates and creates a bwd-implication
(-->) :: Pred -> Pred -> Pred
b1 --> b2 = b1 `BwdImplication` b2

-- Used after (-->) to output the logical implication
emitFactImpl :: Pred -> M ()
emitFactImpl p = case p of
    BwdImplication _ _ -> addPred p
    _ -> error "Not a BwdImplication"


--TODO check that Pred doesn't already exist
newPred :: Name -> [Type] -> M Pred
newPred s xt = do
    let p = Pred s xt
    addPred p
    return p

outputNames :: Pred -> [Name] -> M ()
outputNames p names = do
    modify $ \st -> st {columnNames = insert p names $ columnNames st}
    return ()
--TODO check that Constructor doesn't already exist
-- Creates a Constructor which can be used in `applyVar` to create an instance of it.
newConstructor :: Name -> [Type] -> Type -> M Const
newConstructor s xt t = do
    let c = Constructor s xt t
    modify (\st -> st { consts = c : consts st})
    return (\vars -> AVar c vars)

-- Special case of `newConstructor`, applies the Constucor to an empty list
-- returns a Var
newEmptyConstructor :: Name -> Type -> M Var
newEmptyConstructor n t = do
    constructor <- newConstructor n [] t
    return $ constructor []

--TODO check that type doesn't already exist
-- Creates a new type which can be used when creating predicates and Constructors
newType :: Name -> M Type
newType t = do
    let ty = Type t
    modify (\st -> st { types = ty : types st})
    return ty


addGame :: Game -> M ()
addGame g = do
    modify (\st -> st { games = g : games st })

addPred :: Pred -> M ()
addPred g = do
    modify (\st -> st { preds = g : preds st})

-- Makes a predicate Persistent in the left side of linear implication (-*)
makePersistent ::  Pred ->  Pred
makePersistent p = Persistent p

players :: [String] -> M (Type, [Var], (Pred,Pred,Pred),Pred)
players names = do
    player <- gets player -- newType "player"
    opp <- newFactTypeWithNames "opp" [player, player] ["_","Opponent"]
    players <- mapM (\n -> newEmptyConstructor n player) (names)
    -- noone <- newEmptyConstructor "free" player
    --initiateOpponents names names opp

    -- TODO more general
    newFact opp [head players, last players]
    newFact opp [last players, head players]

    nps <- nextPlayerStage opp
    return (player,players, nps, opp)
    where
        initiatePlayers [] p = return ()
        initiatePlayers (n:ns) p = do
            newConstructor n [] p
            initiatePlayers ns p

        --initiateOpponents []      n2  opp = return ()
        --initiateOpponents (n1:ns) (n2:n2s) opp = newBinding n1 n2 opp

        --opponentHelper you []      = return ()
        --opponentHelper you (n2:ns) = undefined

-- Creates a stage, returns a StageIdentifier which can be used to create
--      transition between stages with e.g. `fromStageToStage`
stage :: Name -> IsInteractive -> [Implication] -> Var -> M (Pred,Pred,Pred)
stage n isInteractive impls playerVar= do
    player <- gets player
    preToken <- if isInteractive
        then newPred ("pretoken_" ++ n)[player]
        else newPred ("pretoken_" ++ n)[player]
    preToken `outputNames` ["Turn"]
    posToken <- newPred ("postoken_" ++ n)[player]

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
-- Helper function for fromStageToStage
fromStageWith :: (Pred,Pred,Pred) -> Var -> [Pred]
fromStageWith (_,stagePred,posToken) v =
    [stagePred, applyPred posToken [v]]

-- Helper function for fromStageToStage
toStageWith :: (Pred,Pred,Pred) -> Var -> [Pred]
toStageWith (preToken,stagePred,_) v =
    [applyPred preToken [v], stagePred]

-- Creates a transition which takes a player from one succesful stage to another stage
-- A succesful stage being one where the player performed one of the actions in a stage
fromStageToStage :: (Pred,Pred,Pred) -> (Pred,Pred,Pred) -> M ()
fromStageToStage from to = do
    p <- gets player
    pVar<- newBinding p
    transition (show (sndOf3 from) ++ "_to_" ++ show (sndOf3 to))
            $ (from `fromStageWith` pVar)
              -*
              (to `toStageWith` pVar)
-- Creates a transition which takes a player from a failed stage to another stage
-- A Failed stage being one where the player didn't have the requirements for any of the actions.
-- Usually used from winStage to nextPlayerStage
fromFailedStageToStage :: (Pred,Pred,Pred) -> (Pred,Pred,Pred) -> M ()
fromFailedStageToStage from to = do
    p <- gets player
    pVar<- newBinding p
    transition (show (sndOf3 from) ++ "_failed_to_" ++ show (sndOf3 to))
            $ (from `toStageWith` pVar)
              -*
              (to `toStageWith` pVar)

sndOf3 :: (a,b,c) -> b
sndOf3 (_,b,_) = b

--TODO Name should probably be auto-generated
-- Creates a transition between stages.
-- The user Should use `fromStageToStage` or `fromFailedStageToStage`
transition :: Name -> Implication -> M ()
transition n (Implication ls rs) = do
    let impl = Implication (qui : ls) rs
    addGame (Transition n impl)

qui :: Pred
qui = Pred "qui" []

-- Creates the stage which handles giving the next player a token
nextPlayerStage :: Pred -> M (Pred,Pred,Pred)
nextPlayerStage opp = do
    let n = "next_player"

    player <- gets player
    preToken <- newPred ("pretoken_" ++ n)[player]
    posToken <- newPred ("postoken_" ++ n)[player]

    prevPlayer <- newBinding player
    nextPlayer <- newBinding player

    let impls = [Implication
                    [ applyPred preToken [prevPlayer]
                    , applyPred opp [prevPlayer,nextPlayer]
                    ]
                    [ applyPred posToken [nextPlayer]]
                ]
        s = Stage n impls False
    addGame s
    let stagePred = StagePred n
    let res = (preToken, stagePred, posToken)


    return res

-- Takes a Constructor and applied it to a list of Vars, and return an AppliedVar
applyVar :: Constructor -> [Var] -> Var
applyVar c vs = AVar c vs

-- Applies a constructor to a Var n times,
-- useful for recursive constructors such as suc
applyVarTimes :: Const -> Var -> Int -> Var
applyVarTimes s x 0 = x
applyVarTimes s x i = s [(applyVarTimes s x (i-1))]

(<+) :: Var -> Int -> M Var
(<+) v n = do
    (nat, s, z) <- gets nats
    let appliedVar = applyVarTimes s v n
    return appliedVar

--TODO Max 26 Vars currently
--     nbrOfBindings could maybe be reset at end of stages/transitions
--     Or perhaps it should be handled in the backend
-- Returns a Binding that can be patternmatched on.

newBinding :: Type -> M Var
newBinding t = do
    n <- gets nbrOfBindings
    let l = (['A'..] !! n) :[]
    modify (\st -> st {nbrOfBindings = n + 1})
    return $ Binding l t

-- Sets values/patterns to a Pred, and return an AppliedPred
applyPred :: Pred -> [Var] -> Pred
applyPred p vars = ApplyPred p vars

-- Sets the initial stage that given player starts in
initialStageAndPlayer :: (Pred,Pred,Pred) -> Var -> M ()
initialStageAndPlayer (pretoken,StagePred n ,_) startingPlayer = do
    modify (\st -> st { initStage = Just n})
    let a = applyPred pretoken [startingPlayer]
    addAppliedPredsToInit [a]


-- Each Pred in the list needs to be applied,
-- since they need to actually have a value.
addAppliedPredsToInit :: [Pred] -> M ()
addAppliedPredsToInit ps =
    modify (\st -> st {initialPreds = ps ++ initialPreds st})



infix 4 -* -- Lower presedence than ++
-- Linear Implication (called lollipop)
-- Removes the left hand side and gives the right hand side
(-*) :: [Pred] -> [Pred] -> Implication
ps1 -* ps2 = Implication ps1 ps2

--


initNats :: M ()
initNats = do
    nat <- newType "nat"
    z <- newEmptyConstructor "z" nat
    s <- newConstructor "s" [nat] nat
    modify (\st -> st {nats=(nat,s,z)})

-- Initias the LT operator (less-than)
-- Returns the predicate "lt" which needs to be applied to something to be used.
initLT :: M Pred
initLT = do
    (nat,s,z) <- gets nats
    lt <- newFactType "lt" [nat, nat]

    n <- newBinding nat
    m <- newBinding nat
    np1 <- n<+1
    mp1 <- m<+1

    newFact lt [z, np1]
    emitFactImpl $ (applyPred lt [n, m]) --> (applyPred lt [np1,mp1])
    return lt
-- Initias the LTE operator (less-than-or-equal) (<=)
-- Returns the predicate "lte" which needs to be applied to something to be used.
--TODO Make helper function so this isn't a copy pasta of initLT
initLTE :: M Pred
initLTE = do
    (nat,s,z) <- gets nats
    lte <- newFactType "lte" [nat, nat]

    n <- newBinding nat
    m <- newBinding nat
    np1 <- n<+1
    mp1 <- m<+1

    newFact lte [z, n]
    emitFactImpl $ (applyPred lte [n, m]) --> (applyPred lte [np1,mp1])
    return lte

initEQ :: M Pred
initEQ = do
    (nat,s,z) <- gets nats
    eq <- newFactType "eq" [nat, nat]

    n <- newBinding nat
    newFact eq [n, n]
    --     m <- newBinding nat
    --    np1 <- n<+1
    --    mp1 <- m<+1
    -- newFact eq [z,z]
    --emitFactImpl $ (applyPred eq [n, m]) --> (applyPred eq [np1,mp1])
    return eq


-- initCoordEQ :: M Pred
initCoordEQ = do
    eq <-initEQ
    (nat, suc, zero) <- gets nats
    board <- gets board
    let (coordType, coord) = coord_t_c board

    coord_eq <- newFactType "coord_eq" [coordType, coordType]
    coord_eq `outputNames` ["Col/Row","Col/Row"]

    x1 <- newBinding nat
    y1 <- newBinding nat

    newFact coord_eq [coord [x1, y1], coord [x1, y1]]

    return coord_eq

initPlayerAndPieceNotEQ opp = do
    player <- gets player
    board <- gets board
    let piece = piece_t board
    let (playerPieceType, pnp, free) = playerPiece_t_c_free board

    pnp_neq <- newFactType "pnp_neq" [playerPieceType, playerPieceType]

    pc<- newBinding piece
    pc2<- newBinding piece
    p <- newBinding player
    p2 <- newBinding player

    newFact pnp_neq [free, pnp [p, pc]]
    newFact pnp_neq [pnp [p, pc], free]
    emitFactImpl $ (pnp_neq `applyPred` [pnp [p, pc], pnp [p2, pc2]])
               --> (opp `applyPred` [p,p2])

    return pnp_neq


data Board = Board
    { coord_t_c  :: (Type, Const)
    , piece_t :: Type
    , playerPiece_t_c_free :: (Type, Const, Var)
    , player_t_c_free :: (Type, Const, Var)
    , tile_p :: Pred
    }

-- Initializes the board
initBoard :: M Board
initBoard = do
    playertype <- gets player
    pieceType <- newType "piece"

    ( nat,_,_) <- gets nats
    coordType <- newType "coordType"
    coord <- newConstructor "coord" [nat,nat] coordType

    playerPieceType <- newType "playerPieceType"
    -- Player and piece. e.g. Black Queen, White Rook
    playerPiece <- newConstructor "player_piece" [playertype, pieceType] playerPieceType
    -- Player only. e.g. Alice, Bob
    player      <- newConstructor "player_ppt"       [playertype] playerPieceType
    -- Free (tile)
    free <- newEmptyConstructor "free" playerPieceType

    tile <- newPred "tile" [playerPieceType, coordType]
    tile `outputNames` ["player/Piece", "Col/Row"]
    let b = Board
            { coord_t_c  = (coordType, coord)
            , piece_t = pieceType
            , playerPiece_t_c_free = (playerPieceType, playerPiece, free)
            , player_t_c_free = (playerPieceType, player, free)
            , tile_p = tile
            }

    modify $ \st -> st {board = b}
    return b




inARow :: Int -> Var -> M Implication
inARow n playerPiece = do
    inARowColumDiagonalHelper playerPiece [0..n-1] [0..]

inAColumn :: Int -> Var -> M Implication
inAColumn n playerPiece = do
    inARowColumDiagonalHelper playerPiece [0..] [0..n-1]

inADiagonal :: Int -> Var -> M [Implication]
inADiagonal n playerPiece = do
    occupiedUp <- inARowColumDiagonalHelper playerPiece [0..n-1] [0..n-1]
    occupiedDown <- inARowColumDiagonalHelper playerPiece [0..n-1] [n-1..0]

    return $ [occupiedUp, occupiedDown]

-- Var should be an applied Constructor of playerPieceType
inARowColumDiagonalHelper :: Var -> [Int] -> [Int] -> M Implication
inARowColumDiagonalHelper playerPiece cols rows = do
    player <- gets player
    (nat, s, z) <- gets nats

    board <- gets board
    let (coordType, coord) = coord_t_c board
    let tile = tile_p board

    x <- newBinding nat
    y <- newBinding nat

    let occupied = map (\(c,r) -> applyPred tile [playerPiece, coord [applyVarTimes s x c, applyVarTimes s y r]])
                       (zip cols rows)
    return $ Implication occupied []





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
    tell' "\n% CONSTRUCTORS"
    createConsts (reverse $ consts st)
    tell' "\n% PREDS AND BWDS"
    createPreds  (reverse $ preds  st)
    tell' "\n% STAGES AND TRANSITIONS"
    createGames (columnNames st)
                (reverse $ games  st)
    tell' "\n% INITIAL"
    case initStage st of
        Nothing -> return () --TODO give error instead -- error "You must have a initial context"
        Just s -> createInit (Initial s (initialPreds st))


createTypes :: [Type] -> O ()
createTypes ts = mapM_ (\(Type name) -> tell' $ name ++ " : type.") ts



createPreds :: [Pred] -> O ()
createPreds = mapM_ (\p -> do
    createPred p
    tell' "."
    )
    where
        createPred :: Pred -> O ()
        createPred = \case
            Pred name ts -> helper name ts "pred"
            Bwd name ts -> helper name ts "bwd"
            BwdImplication b1 b2 -> do
                createPred b2
                tell "\n\t"
                tell "<- "
                createPred b1
            StagePred _ -> error "You can't initialize a StagePred, don't put it in the state"
            ApplyPred p vars -> case p of
                Bwd n ts -> do
                    appliedPred <- checkVars n ts vars
                    tell $ appliedPred
                _ -> error "You can't apply a predicate to a variable in the top level"


helper :: Name -> [Type] -> Name -> O ()
helper name ts right =
    let ts' = map (\(Type a) -> a) ts
    in tell $ name ++ ' ':(intercalate " " ts') ++ " : " ++ right

createConsts :: [Constructor] -> O ()
createConsts cs = mapM_ (\(Constructor n ts t) -> do
                            helper n ts (show t)
                            tell' ".")
                        cs

--TODO Test
createGames :: Map Pred [Name] -> [Game] -> O ()
createGames colnames= mapM_ createGame
    where
        createGame :: Game -> O ()
        createGame = \case
            Stage n impls isInteractive -> do
                tell' $ "stage " ++ n ++ " = {"
                mapM_ (\(impl , ident) -> do
                            when isInteractive $ do
                                implicationsColNames ident impl
                            tell' ident
                            createImplication impl
                      )
                      (zip impls
                           (map (\i -> n ++ '/' : show i) ([1..] :: [Integer]))
                      )
                tell' "}"
                when isInteractive $ tell' $ "#interactive " ++ n ++ "."
                tell' ""
            Transition n impl -> do
                tell' n
                createImplication impl
                tell' ""

        createImplication :: Implication -> O ()
        createImplication (Implication ls rs) = do
                tell "\t: "
                ls' <- mapM createAppliedPred ls
                tell' $ intercalate "\n\t* " ls'
                tell "\t-o "
                rs' <- mapM createAppliedPred rs
                tell $ intercalate "\n\t* " rs'
                tell' "."

        implicationsColNames :: String -> Implication -> O ()
        implicationsColNames ident (Implication ls _) = do
            let res = foldl (predColNames) [] ls
                colns = [cn | (b,cn) <- res]
            tell' $ "%% " ++ ident ++ " " ++ (intercalate " " colns)
            return ()

        predColNames :: [(Name,String)] -> Pred -> [(Name,String)]
        predColNames context = \case
            ApplyPred p vars -> let
                helper cns =
                    foldl (\ctx vc ->
                        case bindingAndColname vc of
                            Just (b,c) ->
                                if b `elem` map fst ctx
                                    then ctx
                                    else  ctx ++ [(b,c)]
                            Nothing -> ctx
                        )
                        context
                        (zip vars cns)
                in
                case lookup p colnames of
                    Just cns -> helper cns
                    Nothing -> helper (repeat "_")
            _ -> context
            where
                bindingAndColname :: (Var, String) -> Maybe (Name, String)
                bindingAndColname (v,cname) = case v of
                        Binding n _ -> Just (n,cname)
                        AVar _ [] -> Nothing
                        AVar _ (v':vs) -> bindingAndColname (v',intercalate " " $ splitOn "/" cname) -- TODO use vs, split cname on /

-- Create the ceptre string from a Pred
--TODO Test
checkVars:: Name -> [Type] -> [Var] -> O String
checkVars n ts vars = let
    checkVar :: Type -> Var -> O String
    checkVar t = \case
        Binding n tp ->
            if (t /= tp)
            then error "Wrong type when applying"
            else return n
        AVar (Constructor nc ts tc) vars -> do
            when (t /= tc) $ error $ "Wrong type when applying Pred " ++ n ++ " to Constructor " ++ nc ++ " with Vars " ++ show vars
            checkedvars <- zipWithM checkVar ts vars
            if checkedvars == []
            then return nc
            else let checkedvars' = intercalate " " checkedvars
                 in return $ "(" ++ nc ++ " " ++ checkedvars' ++ ")"
    in do
    when (length ts /= length vars) $ error $ "Wrong number of vars applied to a pred." ++ " Pred: " ++ n ++ ", Vars:" ++ show vars
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
    Persistent p -> do
        p' <- createAppliedPred p
        return ('$':p')

    BwdImplication _ _ -> error "Can't create BwdImplication in transition/stage"


--TODO Test
createInit :: Initial -> O ()
createInit (Initial n ps) = do
    tell "#trace _ "
    tell n
    tell "\n\t{ "
    appliedPreds <- mapM createAppliedPred ps
    tell $ intercalate "\n\t, " appliedPreds
    tell "}."
