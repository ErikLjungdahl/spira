{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-} -- Warnings enabled

--TODO Only export the functions that we want the user to be able to use.
module Game
    ( compileGame
    -- * Constructors
    , newType
    , newConstructor
    , newEmptyConstructor
    , newPredConstructor
    , newEmptyPred
    , newFactConstructor
    , newBinding
    , makePersistent
    -- ** Functions for Facts
    , (-->)
    , emitFact
    -- ** Predefined types and constructors
    , playerType
    , nat
    , suc
    , zero
    -- * Player creation
    , players
    -- * Stages and transitions
    , (-@)
    , Interactive (Interactive, Noninteractive)
    , stage
    , fromStageToStage
    , fromFailedStageToStage
    , nextPlayerStage
    , initDrawStage
    -- * Initial values
    , initialStageAndPlayer
    , addPredsToInit
    , addPredToInit
    , addToInitialBoard
    -- * Initalize helper functions in Ceptre
    , initLT
    , initLTE
    , initEQ
    , initCoordEQ
    , initPlayerAndPieceNotEQ
    -- * The Board and its record functions
    , initSimpleBoard
    , Board
    , coord_t_c
    , free_v
    , tile_p
    -- ** Extended board
    , initBoard
    , piece_t
    , playerPiece_t_c
    -- * Helper functions for the board
    , inALine
    , inARow
    , inAColumn
    , inADiagonal
    , addToInitialBoard
    -- * UI functions
    , outputNames
    -- * utility functions
    , applyVarTimes
    , (<+)
    , M
    , module Data --  TODO only export some functions in the records
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
-- | Runs the DSL and compiles it to Ceptre
compileGame :: M () -> FilePath -> IO ()
compileGame g fp =
    let ceptreOutput = compile finalSt
        (_, finalSt) = runState (g >> boardToInit ) initSt
        initSt = St
            { types  = [playerType, nat]
            , preds  = []
            , consts = [sucConst,zeroConst]
            , games  = []
            , initStage = Nothing
            , initialPreds = []
            , nbrOfBindings = 0
            , columnNames = Map.empty
            , initialBoard = Map.empty
            }
--        default_config :: M ()
--        default_config = do

        boardToInit :: M ()
        boardToInit = do
            initialBoard <- gets initialBoard
            foldM (\_ -> addPredToInit) () initialBoard

    in writeFile fp ceptreOutput

-- * Constructors
-- TODO Are there other characters allowed?
guardIsLowerCase :: Name -> String -> M ()
guardIsLowerCase n str = do
    unless (head n `elem` ['a'..'z']) $
        error $ "Invalid " ++ str ++ " name: " ++ n ++  " - Must begin with lowercase letter"
    unless (all (`elem` ('_':['a'..'z']++['A'..'Z'])) n) $
        error $ "Invalid " ++ str ++ " name: " ++ n ++  " - Only letters a-z and _ are accepted"

--TODO check that type doesn't already exist
-- | Creates a new type which can be used when creating predicates and Constructors
newType :: Name -> M Type
newType n = do
    guardIsLowerCase n "Type"

    let ty = Type n
    modify (\st -> st { types = ty : types st})
    return ty

--TODO check that Constructor doesn't already exist
-- | Creates a Constructor once applies can be used in predicates
newConstructor :: Name -> [Type] -> Type -> M Const
newConstructor n xt t = do
    guardIsLowerCase n "Constructor"

    let c = Constructor n xt t
    modify (\st -> st { consts = c : consts st})
    return (\vars -> AVar c vars)

-- | Special case of `newConstructor`, applies the Constucor to an empty list
  -- returns a Var
newEmptyConstructor :: Name -> Type -> M Var
newEmptyConstructor n t = do
    constructor <- newConstructor n [] t
    return $ constructor []

--TODO check that Pred doesn't already exist
-- | Creates a new predicate
newPredConstructor :: Name -> [Type] -> M ([Var] -> Pred)
newPredConstructor n xt = do
    guardIsLowerCase n "Pred"

    let p = Pred n xt
    addPred p
    return $ \vars -> ApplyPred p vars

-- | Creates a new empty predicate
newEmptyPred :: Name -> M Pred
newEmptyPred s = do
    p <- newPredConstructor s []
    return $ p []

-- | Creates a Fact constructor
newFactConstructor :: Name -> [Type] -> M ([Var] -> Pred)
newFactConstructor n tx = do
    guardIsLowerCase n "FactConstructor"

    let p = Bwd n tx
    addPred p
    return $ \vars -> ApplyPred p vars

--TODO Max 26 Vars currently, add more
--     nbrOfBindings could maybe be reset at end of stages/transitions
--     Or perhaps it should be handled in the backend
-- | Returns a Binding that can be patternmatched on.
newBinding :: Type -> M Var
newBinding t = do
    n <- gets nbrOfBindings
    let l = (['A'..] !! n) :[]
    modify (\st -> st {nbrOfBindings = n + 1})
    return $ Binding l t




-- * Functions for Facts

-- | Takes two ApplyPreds on Bwd-predicates and creates a bwd-implication
(-->) :: Pred -> Pred -> Pred
b1 --> b2 = b1 `BwdImplication` b2

-- | Used after (-->) to output the logical implication
emitFact :: Pred -> M ()
emitFact p = case p of
    BwdImplication _ _ -> addPred p
    ApplyPred (Bwd _ _) _ -> addPred p
    _ -> error "Not a Fact"



addGame :: Game -> M ()
addGame g = do
    modify (\st -> st { games = g : games st })

addPred :: Pred -> M ()
addPred g = do
    modify (\st -> st { preds = g : preds st})

-- | Makes a predicate Persistent in the left side of linear implication (-@)
makePersistent ::  Pred ->  Pred
makePersistent p = Persistent p


playerType :: Type
playerType = Type "player"


-- | Creates all the players and returns their identifiers along with a stage identifier for the next player's stage
  -- Returns the opposite players.
players :: [String] -> M ([Var], StageIdentifier , ([Var] -> Pred))
players names = do
    opp <- newFactConstructor "opp" [playerType, playerType]
    players <- mapM (\n -> newEmptyConstructor n playerType) (names)
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


-- * Stages and transitions

infix 4 -@ -- Lower presedence than ++

-- | Linear Implication (called lollipop)
  -- Removes the left hand side and gives the right hand side
(-@) :: [Pred] -> [Pred] -> Implication
ps1 -@ ps2 = Implication ps1 ps2

-- | Joines the left and right hand sides, respectively.
  -- Might be useful to combine simple implications to make more complex things
join :: Implication -> Implication -> Implication
join (Implication l1 r1) (Implication l2 r2) = Implication (l1 ++ l2) (r1 ++ r2)


-- Added for clearer type signature
data Interactive = Interactive
                 | Noninteractive
     deriving (Eq)

-- | Creates a stage, returns a StageIdentifier which can be used to create
--      transition between stages with e.g. `fromStageToStage`
stage :: Name -> Interactive -> Var -> [Implication] -> M StageIdentifier
stage n isInteractive playerVar impls = let
        boolInteractive = isInteractive == Interactive
    in do
    guardIsLowerCase n "Stage"


    preToken <- newPredConstructor ("pretoken_" ++ n)[playerType]
    preToken `outputNames` ["Turn"]
    posToken <- newPredConstructor ("postoken_" ++ n)[playerType]

    let impls' = map (\(Implication l r) ->
                        Implication
                            (preToken [playerVar] : l)
                            (posToken [playerVar] : r)
                     )
                     impls
    addGame (Stage n impls' boolInteractive)
    return (preToken, StagePred n, posToken)


-- | Creates a transition which takes a player from one succesful stage to another stage
  -- A succesful stage being one where the player performed one of the actions in a stage
fromStageToStage :: StageIdentifier -> StageIdentifier -> M ()
fromStageToStage from to = do
    pVar<- newBinding playerType
    transition (show (sndOf3 from) ++ "_to_" ++ show (sndOf3 to))
            $ (from `fromStageWith` pVar)
              -@
              (to `toStageWith` pVar)
-- | Creates a transition which takes a player from a failed stage to another stage
  -- A Failed stage being one where the player didn't have the requirements for any of the actions.
  -- Usually used from winStage to nextPlayerStage
fromFailedStageToStage :: StageIdentifier -> StageIdentifier -> M ()
fromFailedStageToStage from to = do
    pVar<- newBinding playerType
    transition (show (sndOf3 from) ++ "_failed_to_" ++ show (sndOf3 to))
            $ (from `toStageWith` pVar)
              -@
              (to `toStageWith` pVar)

-- | Helper function for fromStageToStage
fromStageWith :: StageIdentifier -> Var -> [Pred]
fromStageWith (_,stagePred,posToken) v =
    [stagePred, posToken [v]]

-- | Helper function for fromStageToStage
toStageWith :: StageIdentifier -> Var -> [Pred]
toStageWith (preToken,stagePred,_) v =
    [preToken [v], stagePred]

--TODO Name should probably be auto-generated
-- | Creates a transition between stages.
  -- The user Should use `fromStageToStage` or `fromFailedStageToStage`
transition :: Name -> Implication -> M ()
transition n (Implication ls rs) = do
    guardIsLowerCase n "Transition"

    let impl = Implication (qui : ls) rs
    addGame (Transition n impl)

	where
		qui :: Pred
		qui = Pred "qui" []

-- | Generates the stage for draws
initDrawStage :: M StageIdentifier
initDrawStage = do
  varPlayer <- newBinding playerType
  draw <- newEmptyPred "draw"

  drawStage <- stage "draw" Noninteractive varPlayer [[] -@ [draw]]
  return drawStage


fstOf3 :: (a,b,c) -> a
fstOf3 (a,_,_) = a
sndOf3 :: (a,b,c) -> b
sndOf3 (_,b,_) = b
trdOf3 :: (a,b,c) -> c
trdOf3 (_,_,c) = c



-- | Creates the stage which handles giving the next player a token
nextPlayerStage :: ([Var] -> Pred) -> M StageIdentifier
nextPlayerStage opp = do
    let n = "next_player"

    preToken <- newPredConstructor ("pretoken_" ++ n)[playerType]
    posToken <- newPredConstructor ("postoken_" ++ n)[playerType]

    prevPlayer <- newBinding playerType
    nextPlayer <- newBinding playerType

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

--

-- | Applies a constructor to a Var n times,
  -- useful for recursive constructors such as suc
applyVarTimes :: Const -> Var -> Int -> Var
applyVarTimes _ x 0 = x
applyVarTimes s x i = s [(applyVarTimes s x (i-1))]

(<+) :: Var -> Int -> Var
(<+) v n = applyVarTimes suc v n


-- * Initial values

-- |Sets the initial stage that given player starts in
initialStageAndPlayer :: StageIdentifier -> Var -> M ()
initialStageAndPlayer (pretoken,StagePred n ,_) startingPlayer = do
    modify (\st -> st { initStage = Just n})
    let a = pretoken [startingPlayer]
    addPredToInit a
initialStageAndPlayer _ _ = error "Invalid StageIdentifier"

-- | Each Pred in the list needs to be applied,
  -- since they need to actually have a value.
addPredsToInit :: [Pred] -> M ()
addPredsToInit = mapM_ addPredToInit

addPredToInit :: Pred -> M ()
addPredToInit p = modify (\st -> st {initialPreds = p : initialPreds st})

addToInitialBoard :: Pred -> M ()
addToInitialBoard p = case p of
    ApplyPred (Pred "tile" _) (_:coord:[]) -> do
        board <- gets initialBoard
        let updatedBoard = Map.insert coord p board
        modify $ \st -> st {initialBoard = updatedBoard}
    _ -> error "Can't add non-tile Pred to InitialBoard"

-- * Initalize helper functions in Ceptre

--  Initializes the natural numbers, successor and zero constructors.

nat :: Type
nat = Type "nat"

suc :: Const
suc = (\vars -> AVar sucConst vars)

sucConst :: Constructor
sucConst = Constructor "s" [nat] nat

zero :: Var
zero = AVar zeroConst []

zeroConst :: Constructor
zeroConst = Constructor "z" [] nat

-- | Initializes the less-than-operator (<)
  -- Returns the predicate "lt" which needs to be applied to something to be used.
initLT :: M ([Var] -> Pred)
initLT = do
    lt <- newFactConstructor "lt" [nat, nat]

    n <- newBinding nat
    m <- newBinding nat

    emitFact $ lt [zero, n<+1]
    emitFact $ (lt [n, m]) --> (lt [n<+1,m<+1])
    return lt

-- | Initializes the less-than-or-equal-operator (<=)
  -- Returns the predicate "lte" which needs to be applied to something to be used.
--TODO Make helper function so this isn't a copy pasta of initLT
initLTE :: M ([Var] -> Pred)
initLTE = do
    lte <- newFactConstructor "lte" [nat, nat]

    n <- newBinding nat
    m <- newBinding nat

    emitFact $ lte [zero, n]
    emitFact $ (lte [n, m]) --> (lte [n<+1,m<+1])
    return lte

-- | Initializes the equal operator (==)
initEQ :: M ([Var] -> Pred)
initEQ = do
    eq <- newFactConstructor "eq" [nat, nat]

    n <- newBinding nat
    emitFact $ eq [n, n]
    --     m <- newBinding nat
    -- emitFact eq [z,z]
    --emitFact $ (eq [n, m]) --> (eq [n<+1,m<+1])
    return eq

-- | Initializes the not equal operator (/=)
initNEQ :: M ([Var] -> Pred)
initNEQ = do
    neq <- newFactConstructor "neq" [nat, nat]

    n <- newBinding nat
    m <- newBinding nat

    emitFact $ neq [zero, n<+1]
    emitFact $ neq [n<+1, zero]
    emitFact $ (neq [n, m]) --> (neq [n<+1,m<+1])
    return neq



-- | Initializes the equal operator for coordinates (==)
initCoordEQ :: M ([Var] -> Pred)
initCoordEQ = do
    -- eq <-initEQ
    board <- gets board
    let (coordType, coord) = coord_t_c board

    coord_eq <- newFactConstructor "coord_eq" [coordType, coordType]
    coord_eq `outputNames` ["Col/Row","Col/Row"]

    x1 <- newBinding nat
    y1 <- newBinding nat

    emitFact $ coord_eq [coord [x1, y1], coord [x1, y1]]

    return coord_eq

-- | Initializes the not-equal operator (/=) for the "Player and Piece Constructor"
-- which is used for the tiles of the board
initPlayerAndPieceNotEQ :: ([Var] -> Pred) -> M ([Var] -> Pred)
initPlayerAndPieceNotEQ opp = do
    board <- gets board
    let piece = piece_t board
    let (playerPieceType, pnp) = playerPiece_t_c board
        free = free_v board

    pnp_neq <- newFactConstructor "pnp_neq" [playerPieceType, playerPieceType]

    pc<- newBinding piece
    pc2<- newBinding piece
    p <- newBinding playerType
    p2 <- newBinding playerType

    emitFact $ pnp_neq [free, pnp [p, pc]]
    emitFact $ pnp_neq [pnp [p, pc], free]
    emitFact $ (pnp_neq [pnp [p, pc], pnp [p2, pc2]])
               --> (opp [p,p2])

    return pnp_neq


-- | Initializes the board, with the tiles containing a player and piece
  -- Sets all tiles to free. To add a playerPiece to the initialBoard, use @addToInitialBoard
initBoard :: Int -> Int -> M Board
initBoard cols rows = do
    pieceType <- newType "piece"
    playerPieceType <- newType "playerPieceType"
    -- Player and piece. e.g. Black Queen, White Rook
    playerPiece <- newConstructor "player_piece" [playerType, pieceType] playerPieceType

    b <- initBoardHelper cols rows playerPieceType
    tile_p b `outputNames` ["player/Piece", "Col/Row"]

    let b' = b { piece_t = pieceType
               , playerPiece_t_c = (playerPieceType, playerPiece)
               }
    modify $ \st -> st {board = b'}
    return b'


-- | Initializes the board, with the tiles containing just a player.
  -- Sets all tiles to free. To add a playerPiece to the initialBoard, use @addToInitialBoard
initSimpleBoard :: Int -> Int -> M Board
initSimpleBoard cols rows = do
    b <- initBoardHelper cols rows playerType
    tile_p b `outputNames` ["player", "Col/Row"]

    modify $ \st -> st {board = b}
    return b


initBoardHelper :: Int -> Int -> Type -> M Board
initBoardHelper cols rows playerPieceType = do
    coordType <- newType "coordType"
    coord <- newConstructor "coord" [nat,nat] coordType

    -- Free (tile)
    free <- newEmptyConstructor "free" playerPieceType

    tile <- newPredConstructor "tile" [playerPieceType, coordType]
    let b = Board
            { coord_t_c  = (coordType, coord)
            , free_v = free
            , tile_p = tile
            }

    mapM_ (\c ->
            addToInitialBoard $
                tile [free, c]
         )
         [coord [applyVarTimes suc zero x ,applyVarTimes suc zero y]
            | x <- [0..cols-1], y <- [0..rows-1] ]

    return b


-- * Helper functions for the board

-- | Combines inARow, inAColumn, inADiagonal
inALine :: Int -> Var -> M [Implication]
inALine n playerPiece = do
    rowrule <- inARow    n playerPiece
    colrule <- inAColumn n playerPiece
    diarules <- inADiagonal n playerPiece
    return $ rowrule:colrule:diarules

-- | Checks that a specific player piece is n times in a row
inARow :: Int -> Var -> M Implication
inARow n playerPiece = do
    inARowColumDiagonalHelper playerPiece [0..n-1] (repeat 0)

-- | Checks that a specific player piece is n times in a column
inAColumn :: Int -> Var -> M Implication
inAColumn n playerPiece = do
    inARowColumDiagonalHelper playerPiece (repeat 0) [0..n-1]

-- | Checks that a specific player piece is n times in a diagonal
inADiagonal :: Int -> Var -> M [Implication]
inADiagonal n playerPiece = do
    occupiedUp <- inARowColumDiagonalHelper playerPiece [0..n-1] [0..n-1]
    occupiedDown <- inARowColumDiagonalHelper playerPiece [0..n-1] (reverse [0..n-1])

    return $ [occupiedUp, occupiedDown]

-- | Var should be an applied Constructor of playerPieceType
inARowColumDiagonalHelper :: Var -> [Int] -> [Int] -> M Implication
inARowColumDiagonalHelper playerPiece cols rows = do
    board <- gets board
    let (_, coord) = coord_t_c board
    let tile = tile_p board

    x <- newBinding nat
    y <- newBinding nat

    let occupied = map (\(c,r) -> tile [playerPiece, coord [applyVarTimes suc x c, applyVarTimes suc y r]])
                       (zip cols rows)
    return $ Implication occupied []


-- TODO Typecheck the columnnames, right amount of arguments etc
-- | Outputs names of the variables during runtime
outputNames :: ([Var] -> Pred) -> [Name] -> M ()
outputNames fp names = do
    let ApplyPred p _ = fp []
    modify $ \st -> st {columnNames = Map.insert p names $ columnNames st}
    return ()
