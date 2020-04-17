{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-} -- Warnings enabled

--TODO Only export the functions that we want the user to be able to use.
module Game
    ( module Game
    , module Data
    , module Control.Monad.State
    ) where

-- Our modules
import Data
import Compiler

-- Libraries
import Prelude hiding (pred, init, lookup)
import Control.Monad.State
import qualified Data.Map.Lazy as Map (insert, empty)
import Data.Map.Lazy (Map)

-- For debugging
--import Debug.Trace


type M a = State St a

runGame :: M () -> FilePath -> IO ()
runGame g fp =
    let ceptreOutput = compile finalSt
        (_, finalSt) = runState (default_config >> g >> boardToInit ) initSt
        initSt = St
            { types  = []
            , preds  = []
            , consts = []
            , games  = []
            , initStage = Nothing
            , initialPreds = []
            , nbrOfBindings = 0
            , columnNames = Map.empty
            , initialBoard = Map.empty
            }
        default_config :: M ()
        default_config = do
            player <- newType "player"
            -- turn_p <- newPred "turn" [player]

            modify (\st -> st {playerType = player})
            initNats

        boardToInit :: M ()
        boardToInit = do
            initialBoard <- gets initialBoard
            foldM (\_ -> addPredToInit) () initialBoard

    in writeFile fp ceptreOutput

-- Generates the stage for draws
initDrawStage :: M StageIdentifier
initDrawStage = do
    player <- gets playerType
    varPlayer <- newBinding player
    draw <- newEmptyPred "draw"

    drawStage <- stage "draw" False [[] -* [draw]] varPlayer
    modify (\st -> st {drawStage = drawStage})
    return drawStage

-- | Creates a Fact constructor
newFactConstructor :: String -> [Type] -> M ([Var] -> Pred)
newFactConstructor s tx = do
    let p = Bwd s tx
    addPred p
    return $ \vars -> ApplyPred p vars
--
-- Takes two ApplyPreds on Bwd-predicates and creates a bwd-implication
(-->) :: Pred -> Pred -> Pred
b1 --> b2 = b1 `BwdImplication` b2

-- Used after (-->) to output the logical implication
emitFact :: Pred -> M ()
emitFact p = case p of
    BwdImplication _ _ -> addPred p
    ApplyPred (Bwd _ _) _ -> addPred p
    _ -> error "Not a Fact"


--TODO check that Pred doesn't already exist
newPred :: Name -> [Type] -> M ([Var] -> Pred)
newPred s xt = do
    let p = Pred s xt
    addPred p
    return $ \vars -> ApplyPred p vars

newEmptyPred :: Name -> M Pred
newEmptyPred s = do
    p <- newPred s []
    return $ p []

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
players :: [String] -> M ([Var], StageIdentifier , ([Var] -> Pred))
players names = do
    player <- gets playerType -- newType "player"
    opp <- newFactConstructor "opp" [player, player]
    players <- mapM (\n -> newEmptyConstructor n player) (names)
    -- noone <- newEmptyConstructor "free" player
    --initiateOpponents names names opp

    -- TODO more general
    emitFact $ opp [head players, last players]
    emitFact $ opp [last players, head players]

    nps <- nextPlayerStage opp
    return (players, nps, opp)
    --where
    --    initiatePlayers [] p = return ()
    --    initiatePlayers (n:ns) p = do
    --        newConstructor n [] p
    --        initiatePlayers ns p

        --initiateOpponents []      n2  opp = return ()
        --initiateOpponents (n1:ns) (n2:n2s) opp = newBinding n1 n2 opp

        --opponentHelper you []      = return ()
        --opponentHelper you (n2:ns) = undefined

-- Creates a stage, returns a StageIdentifier which can be used to create
--      transition between stages with e.g. `fromStageToStage`
stage :: Name -> IsInteractive -> [Implication] -> Var -> M StageIdentifier
stage n isInteractive impls playerVar= do
    player <- gets playerType
    preToken <- if isInteractive
        then newPred ("pretoken_" ++ n)[player]
        else newPred ("pretoken_" ++ n)[player]
    preToken `outputNames` ["Turn"]
    posToken <- newPred ("postoken_" ++ n)[player]

    let appliedPreToken =  preToken [playerVar]
        appliedPosToken =  posToken [playerVar]
        impls' = map (\(Implication l r) -> Implication
                                                (appliedPreToken : l)
                                                (appliedPosToken : r) )
                      impls
        s = Stage n impls' isInteractive
    addGame s
    let stagePred = StagePred n
    let res = (preToken, stagePred, posToken)

    -- Add draw upon failed stage
    -- should probably be up to the user. Not all games ends in a draw when someone can't do something
    --when isInteractive $ do
    --    drawStage <- gets drawStage
    --    transition (n ++ "_to_draw")
    --               ((res `toStageWith` playerVar)
    --               -*
    --                (drawStage `toStageWith` playerVar))

    return res
-- Helper function for fromStageToStage
fromStageWith :: StageIdentifier -> Var -> [Pred]
fromStageWith (_,stagePred,posToken) v =
    [stagePred, posToken [v]]

-- Helper function for fromStageToStage
toStageWith :: StageIdentifier -> Var -> [Pred]
toStageWith (preToken,stagePred,_) v =
    [preToken [v], stagePred]

-- Creates a transition which takes a player from one succesful stage to another stage
-- A succesful stage being one where the player performed one of the actions in a stage
fromStageToStage :: StageIdentifier -> StageIdentifier -> M ()
fromStageToStage from to = do
    p <- gets playerType
    pVar<- newBinding p
    transition (show (sndOf3 from) ++ "_to_" ++ show (sndOf3 to))
            $ (from `fromStageWith` pVar)
              -*
              (to `toStageWith` pVar)
-- Creates a transition which takes a player from a failed stage to another stage
-- A Failed stage being one where the player didn't have the requirements for any of the actions.
-- Usually used from winStage to nextPlayerStage
fromFailedStageToStage :: StageIdentifier -> StageIdentifier -> M ()
fromFailedStageToStage from to = do
    p <- gets playerType
    pVar<- newBinding p
    transition (show (sndOf3 from) ++ "_failed_to_" ++ show (sndOf3 to))
            $ (from `toStageWith` pVar)
              -*
              (to `toStageWith` pVar)
fstOf3 :: (a,b,c) -> a
fstOf3 (a,_,_) = a
sndOf3 :: (a,b,c) -> b
sndOf3 (_,b,_) = b
trdOf3 :: (a,b,c) -> c
trdOf3 (_,_,c) = c

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
nextPlayerStage :: ([Var] -> Pred) -> M StageIdentifier
nextPlayerStage opp = do
    let n = "next_player"

    player <- gets playerType
    preToken <- newPred ("pretoken_" ++ n)[player]
    posToken <- newPred ("postoken_" ++ n)[player]

    prevPlayer <- newBinding player
    nextPlayer <- newBinding player

    let impls = [Implication
                    [ preToken [prevPlayer]
                    , opp [prevPlayer,nextPlayer]
                    ]
                    [ posToken [nextPlayer]]
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
applyVarTimes _ x 0 = x
applyVarTimes s x i = s [(applyVarTimes s x (i-1))]

(<+) :: Var -> Int -> M Var
(<+) v n = do
    (_, s, _) <- gets nats
    let appliedVar = applyVarTimes s v n
    return appliedVar

--TODO Max 26 Vars currently, add more
--     nbrOfBindings could maybe be reset at end of stages/transitions
--     Or perhaps it should be handled in the backend
-- Returns a Binding that can be patternmatched on.

newBinding :: Type -> M Var
newBinding t = do
    n <- gets nbrOfBindings
    let l = (['A'..] !! n) :[]
    modify (\st -> st {nbrOfBindings = n + 1})
    return $ Binding l t


-- Sets the initial stage that given player starts in
initialStageAndPlayer :: StageIdentifier -> Var -> M ()
initialStageAndPlayer (pretoken,StagePred n ,_) startingPlayer = do
    modify (\st -> st { initStage = Just n})
    let a = pretoken [startingPlayer]
    addPredToInit a
initialStageAndPlayer _ _ = error "Invaldig StageIdentifier"


-- Each Pred in the list needs to be applied,
-- since they need to actually have a value.
addAppliedPredsToInit :: [Pred] -> M ()
addAppliedPredsToInit = mapM_ addPredToInit

addPredToInit :: Pred -> M ()
addPredToInit p = modify (\st -> st {initialPreds = p : initialPreds st})



addToInitialBoard :: Pred -> M ()
addToInitialBoard p = case p of
    ApplyPred (Pred "tile" _) (_:coord:[]) -> do
        board <- gets initialBoard
        let updatedBoard = Map.insert coord p board
        modify $ \st -> st {initialBoard = updatedBoard}
    _ -> error "Can't add non-tile Pred to InitialBoard"



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
initLT :: M ([Var] -> Pred)
initLT = do
    (nat,_,z) <- gets nats
    lt <- newFactConstructor "lt" [nat, nat]

    n <- newBinding nat
    m <- newBinding nat
    np1 <- n<+1
    mp1 <- m<+1

    emitFact $ lt [z, np1]
    emitFact $ (lt [n, m]) --> (lt [np1,mp1])
    return lt
-- Initias the LTE operator (less-than-or-equal) (<=)
-- Returns the predicate "lte" which needs to be applied to something to be used.
--TODO Make helper function so this isn't a copy pasta of initLT
initLTE :: M ([Var] -> Pred)
initLTE = do
    (nat,_,z) <- gets nats
    lte <- newFactConstructor "lte" [nat, nat]

    n <- newBinding nat
    m <- newBinding nat
    np1 <- n<+1
    mp1 <- m<+1

    emitFact $ lte [z, n]
    emitFact $ (lte [n, m]) --> (lte [np1,mp1])
    return lte

initEQ :: M ([Var] -> Pred)
initEQ = do
    (nat,_,_) <- gets nats
    eq <- newFactConstructor "eq" [nat, nat]

    n <- newBinding nat
    emitFact $ eq [n, n]
    --     m <- newBinding nat
    --    np1 <- n<+1
    --    mp1 <- m<+1
    -- emitFact eq [z,z]
    --emitFact $ (eq [n, m]) --> (eq [np1,mp1])
    return eq


initCoordEQ :: M ([Var] -> Pred)
initCoordEQ = do
    -- eq <-initEQ
    (nat, _, _) <- gets nats
    board <- gets board
    let (coordType, coord) = coord_t_c board

    coord_eq <- newFactConstructor "coord_eq" [coordType, coordType]
    coord_eq `outputNames` ["Col/Row","Col/Row"]

    x1 <- newBinding nat
    y1 <- newBinding nat

    emitFact $ coord_eq [coord [x1, y1], coord [x1, y1]]

    return coord_eq

initPlayerAndPieceNotEQ :: ([Var] -> Pred) -> M ([Var] -> Pred)
initPlayerAndPieceNotEQ opp = do
    player <- gets playerType
    board <- gets board
    let piece = piece_t board
    let (playerPieceType, pnp, free) = playerPiece_t_c_free board

    pnp_neq <- newFactConstructor "pnp_neq" [playerPieceType, playerPieceType]

    pc<- newBinding piece
    pc2<- newBinding piece
    p <- newBinding player
    p2 <- newBinding player

    emitFact $ pnp_neq [free, pnp [p, pc]]
    emitFact $ pnp_neq [pnp [p, pc], free]
    emitFact $ (pnp_neq [pnp [p, pc], pnp [p2, pc2]])
               --> (opp [p,p2])

    return pnp_neq




-- Initializes the board
-- Sets all tiles to free. To add a playerPiece to the initialBoard, use @addToInitialBoard
initBoard :: Int -> Int -> M Board
initBoard cols rows = do
    playertype <- gets playerType
    pieceType <- newType "piece"

    ( nat,s,z) <- gets nats
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

    mapM_ (\c ->
            addToInitialBoard $
                tile [free, c]
         )
         [coord [applyVarTimes s z x ,applyVarTimes s z y]
            | x <- [0..cols-1], y <- [0..rows-1] ]

    return b



-- Combines inARow, inAColumn, inADiagonal
inALine :: Int -> Var -> M [Implication]
inALine n playerPiece = do
    rowrule <- inARow    n playerPiece
    colrule <- inAColumn n playerPiece
    diarules <- inADiagonal n playerPiece
    return $ rowrule:colrule:diarules

inARow :: Int -> Var -> M Implication
inARow n playerPiece = do
    inARowColumDiagonalHelper playerPiece [0..n-1] (repeat 0)

inAColumn :: Int -> Var -> M Implication
inAColumn n playerPiece = do
    inARowColumDiagonalHelper playerPiece (repeat 0) [0..n-1]

inADiagonal :: Int -> Var -> M [Implication]
inADiagonal n playerPiece = do
    occupiedUp <- inARowColumDiagonalHelper playerPiece [0..n-1] [0..n-1]
    occupiedDown <- inARowColumDiagonalHelper playerPiece [0..n-1] (reverse [0..n-1])

    return $ [occupiedUp, occupiedDown]

-- Var should be an applied Constructor of playerPieceType
inARowColumDiagonalHelper :: Var -> [Int] -> [Int] -> M Implication
inARowColumDiagonalHelper playerPiece cols rows = do
    (nat, s, _) <- gets nats

    board <- gets board
    let (_, coord) = coord_t_c board
    let tile = tile_p board

    x <- newBinding nat
    y <- newBinding nat

    let occupied = map (\(c,r) -> tile [playerPiece, coord [applyVarTimes s x c, applyVarTimes s y r]])
                       (zip cols rows)
    return $ Implication occupied []

numberType :: St -> Type
numberType = fstOf3 . nats


-- TODO Typecheck the columnnames, right amount of arguments etc
outputNames :: ([Var] -> Pred) -> [Name] -> M ()
outputNames fp names = do
    let ApplyPred p _ = fp []
    modify $ \st -> st {columnNames = Map.insert p names $ columnNames st}
    return ()
