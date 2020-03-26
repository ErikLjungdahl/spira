import Game
import Control.Monad.State

import Prelude hiding ((+))
import Data.List

main :: IO ()
main = runGame chess "game.cep"

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
    rowrule <- inARow    5 occupied p
    colrule <- inAColumn 5 occupied p
    diarules <- inADiagonal 5 occupied p
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
        map (applyPred free) [[applyVarTimes s z x ,applyVarTimes s z y] | x <- [0..19], y <- [0..19]]
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
    six <- zero+6 -- applyVarTimes s zero 6
    newFact maxFact [six]

    -- Pick a free tile and make it occupied by the player
    x <- newBinding nat
    y <- newBinding nat
    p <- newBinding player
    m <- newBinding nat
    yP1 <- y+1
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
    xs <- mapM (zero+) [0..6]
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
    (player, playernames, stage_next_player, opp) <- players ["hugo","musen"]
    piece <- newType "piece"
    horse <- newEmptyConstructor "horse" piece
    nothing  <- newEmptyConstructor "nothing"  piece
    tile <- newPredWithTypeAndNames "tile" [player, piece, nat, nat] ["turn", "piece", "col", "row"]

    let free = head playernames
    x <- newBinding nat
    y <- newBinding nat
    p <- newBinding player
    p2 <- newBinding player
    whatever <- newBinding piece

    let horseImpl (rowB, rowA, colB, colA) =
                [ applyPred opp [p, p2]
                , applyPred tile [p2, whatever, rowA, colA]
                , applyPred tile [p, horse, rowB ,colB]
                ] -*
                [ applyPred tile [p  , horse, rowA, colA]
                , applyPred tile [free, nothing, rowB ,colB]
                ]
    let coordinates =
            [(0,1,0,2)
            ,(0,1,2,0)
            ,(0,2,0,1)
            ,(0,2,1,0)
            ,(1,0,0,2)
            ,(1,0,2,0)
            ,(2,0,0,1)
            ,(2,0,1,0)
            ]
    appliedBindings <- mapM (\(rowB,rowA,colB,colA) -> do
        r1<-x+rowB
        r2<-x+rowA
        r3<-y+colB
        r4<-y+colA
        return (r1,r2,r3,r4)
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
        x'<- zero+x
        y'<- zero+y
        return (x',y')
        ) [(x,y) | x <-[0..7], y <- [0..7]]
    addAppliedPredsToInit $
        map (\(x,y) -> applyPred tile [free, nothing, x ,y]) xys
    three <- zero+3
    addAppliedPredsToInit [(applyPred tile [player1, horse,three,three])]
    return ()
