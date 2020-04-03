import Game
import Control.Monad.State

import Prelude hiding ((+))
import Data.List

main :: IO ()
main = runGame ticTacToe "game.cep"

run :: M () -> IO ()
run g = runGame g "game.cep"

ticTacToe :: M ()
ticTacToe = do
    --nats
    --players ["jennie","simon","erik","peter","nicke","oskar"]
    (nat,s,z) <- gets nats
    (player, playernames, stage_next_player, opp) <- players ["simon","jennie","erik"]

    free <- newPredWithType "free" [nat,nat]
    occupied <- newPredWithType "occupied" [player,nat,nat]

    -- Pick a free tile and make it occupied by the player
    x <- newBinding nat
    y <- newBinding nat
    p <- newBinding player
    let impl = [applyPred free [x,y]] -* [applyPred occupied [p,x,y]]
    stage_play<- stage "play" True [impl] p

    -- A player wins if they have 3 occupied tiles in a row/colum/diagnal
    rowrule <- inARow    3 occupied p
    colrule <- inAColumn 3 occupied p
    diarules <- inADiagonal 3 occupied p
    stage_win <- stage "win" False (rowrule:colrule:diarules) p

    -- After play we check win condition
    stage_play `fromStageToStage` stage_win
    -- If noone has won, we go to the next player
    stage_win `fromFailedStageToStage`stage_next_player
    -- And then the next player gets to play
    stage_next_player`fromStageToStage` stage_play


    initialStageAndPlayer stage_play (head playernames)

    -- Set all tiles to free as initial state
    addAppliedPredsToInit $
        map (applyPred free) [[applyVarTimes s z x ,applyVarTimes s z y] | x <- [0..2], y <- [0..2]]
    return ()


connectFour :: M ()
connectFour = do
    --nats
    --players ["jennie","simon","erik","peter","nicke","oskar"]
    (nat,suc,zero) <- gets nats
    (player, playernames, stage_next_player, opp) <- players ["hugo","musen"]

    free <- newPredWithType "free" [nat,nat]
    occupied <- newPredWithType "occupied" [player,nat,nat]

    lt <- initLT
    maxFact  <- newFactType "max" [nat]
    six <- zero<+6 -- applyVarTimes s zero 6
    newFact maxFact [six]

    -- Pick a free tile and make it occupied by the player
    x <- newBinding nat
    y <- newBinding nat
    p <- newBinding player
    m <- newBinding nat
    yP1 <- y<+1
    let impl = [ applyPred free [x,y]
               , applyPred maxFact [m]
               , applyPred lt [y, m]
               ] -*
               [ applyPred occupied [p,x,y]
               , applyPred free [x,yP1]  -- Makes the tile above free
               ]
    stage_play<- stage "play" True [impl] p

    -- A player wins if they have 4 occupied tiles in a row/colum/diagnal
    rowrule  <- inARow      4 occupied p
    colrule  <- inAColumn   4 occupied p
    diarules <- inADiagonal 4 occupied p
    stage_win <- stage "win" False (rowrule:colrule:diarules) p

    -- After play we check win condition
    stage_play `fromStageToStage` stage_win
    -- If noone has won, we go to the next player
    stage_win `fromFailedStageToStage`stage_next_player
    -- And then the next player gets to play
    stage_next_player`fromStageToStage` stage_play


    initialStageAndPlayer stage_play (head playernames)

    -- Set all tiles to free as initial state
    --
    xs <- mapM (zero<+) [0..6]
    addAppliedPredsToInit $
        map (\x -> applyPred free [x, zero]) xs
    return ()

chess :: M ()
chess = do
{-
piece :  type.
Häst  : piece.
Bonde : piece.

nothing : player.
opp kalle nothing : player.
opp kalle pelle : player.

tile player piece nat nat : pred.

stage move = {
move/1
    : tile P Häst X Y
    -* tile P Häst (s X) (s (s Y))
move/1
    : tile P       Häst       X      Y
    * opp P P2
    * tile P2      _    (s (s X)) (s Y)
    -o tile P      Häst (s (s X)) (s Y)
    *  tile nothing free X Y
-}


    (nat,suc,zero) <- gets nats
    (player, playernames, stage_next_player, oppMaybe) <- players ["hugo","musen"]
    piece <- newType "piece"
    horse <- newEmptyConstructor "horse" piece
    nothing  <- newEmptyConstructor "nothing"  piece
    tile <- newPredWithTypeAndNames "tile" [player, piece, nat, nat] ["Turn", "Piece", "Col", "Row"]

    let free = head playernames
    x <- newBinding nat
    y <- newBinding nat
    p <- newBinding player
    p2 <- newBinding player
    whatever <- newBinding piece

    -- Horses move.
    let horseImpl [(rowB, colB),(rowA, colA)] =
                [ applyPred oppMaybe [p, p2] -- p2 can be either opponent or free tile
                , applyPred tile [p, horse, rowB ,colB]
                , applyPred tile [p2, whatever, rowA, colA]
                ] -*
                [ applyPred tile [p  , horse, rowA, colA]
                , applyPred tile [free, nothing, rowB ,colB]
                ]
    -- The different moves a horse can do, moduled in positive coordinates
    -- TODO Desribe as one coordinate and as mirrors of that, then turn it into positive coordinates
    let coordinates =
        --   Before-After
            [[(0,0),(1,2)]
            ,[(0,2),(1,0)]
            ,[(0,0),(2,1)]
            ,[(0,1),(2,0)]
            ,[(1,0),(0,2)]
            ,[(1,2),(0,0)]
            ,[(2,0),(0,1)]
            ,[(2,1),(0,0)]
            ]
    appliedBindings <- mapM (\cds ->
        mapM (\(r,c) -> do
              a <- x<+r
              b <- y<+c
              return (a,b)
             ) cds
        ) coordinates
    let impls = map horseImpl appliedBindings
    stage_play <- stage "play" True impls p


    -- After play we check win condition
    stage_play `fromStageToStage` stage_play

    let player1 = last playernames
    initialStageAndPlayer stage_play (player1)

    -- Set all tiles to free as initial state
    --
    xys <- mapM (\(x,y) -> do
        x'<- zero<+x
        y'<- zero<+y
        return (x',y')
        ) [(x,y) | x <-[0..7], y <- [0..7]]
    addAppliedPredsToInit $
        map (\(x,y) -> applyPred tile [free, nothing, x ,y]) xys
    three <- zero<+3
    addAppliedPredsToInit [(applyPred tile [player1, horse,three,three])]
    return ()



othello :: M ()
othello = do
    (nat,suc,zero) <- gets nats
    (player, playernames, stage_next_player, opp) <- players ["black","white"]
    noone <- newEmptyConstructor "free" player

    coordtype <- newType "coordtype"
    coord <- newConstructor "coord" [nat,nat] coordtype

    tile <- newPredWithTypeAndNames "tile" [player, coordtype] ["Color", "Col/Row"]
    lastPlaced <- newPredWithType "lastPlaced" [player, coordtype]

    --eq <- initEQ

    -- Create Board

    let black = head playernames
    let white = last playernames

    x <- newBinding nat
    y <- newBinding nat
    --output <- newBinding coord
    p <- newBinding player
    p2 <- newBinding player

    let place [startPos, middlePos, endPos] =
                [                  opp  `applyPred` [p, p2]
                ,                  tile `applyPred` [noone, startPos]
                , makePersistent $ tile `applyPred` [p2, middlePos]
                , makePersistent $ tile `applyPred` [p, endPos]
                --, eq `applyPred` [startPos, coord]
                ] -*
                [            tile       `applyPred` [p, startPos]
                ,            lastPlaced `applyPred` [p, startPos]
                ]
    let coordinates = half ++ map reverse half
            where half =
                    [[(0,0),(1,0),(2,0)]
                    ,[(0,0),(1,1),(2,2)]
                    ,[(0,0),(0,1),(0,2)]
                    ,[(2,0),(1,1),(0,2)]
                    ]
    allPossiblePositions <-
        mapM (\positions ->
            mapM (\pos -> do
                x' <- x <+ fst pos
                y' <- y <+ snd pos
                return $ coord [x',y']
            ) positions
        ) coordinates

    let impls_play = map place (allPossiblePositions)
    stage_play <- stage "play" True impls_play p


    let flip' [startPos, middlePos, endPos] =
                [                  opp        `applyPred` [p, p2]
                , makePersistent $ lastPlaced `applyPred` [p , startPos ]
                ,                  tile       `applyPred` [p2, middlePos]
                , makePersistent $ tile       `applyPred` [p , endPos ]
                ] -*
                [                  tile       `applyPred` [p, middlePos]
                ]
    let impls_flip = map flip' allPossiblePositions
    stage_flip <- stage "flip" False impls_flip p


    stage_remove <- stage "remove_last_player" False [ [lastPlaced `applyPred` [p, coord [x,y]]] -* [] ] p


    stage_play `fromStageToStage` stage_flip
    stage_flip `fromStageToStage` stage_flip
    stage_flip `fromFailedStageToStage` stage_remove
    stage_remove `fromStageToStage` stage_next_player
    stage_next_player `fromStageToStage` stage_play



    xys <- mapM (\(x,y) -> do
        x'<- zero <+ x
        y'<- zero <+ y
        return $ coord [x',y']
        ) [(x,y) | x <-[0..7], y <- [0..7]]
    addAppliedPredsToInit $
        map (\coordinate -> applyPred tile [noone, coordinate]) xys
    three <- zero <+ 3
    four <- zero <+ 4
    addAppliedPredsToInit [(applyPred tile [ black, coord [three,four ] ])
                          ,(applyPred tile [ black, coord [four ,three] ])
                          ,(applyPred tile [ white, coord [three,three] ])
                          ,(applyPred tile [ white, coord [four ,four ] ])
                          ]

    initialStageAndPlayer stage_play black


    return ()
