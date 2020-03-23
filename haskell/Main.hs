import NewGame
import Control.Monad.State

main :: IO ()
main = runGame ticTacToe "game.cep"

ticTacToe :: M ()
ticTacToe = do
    --nats
    --players ["jennie","simon","erik","peter","nicke","oskar"]
    (nat,s,z) <- gets nats
    (player, playernames, stage_next_player) <- players ["simon","jennie","erik"]

    free <- newPredWithType "free" [nat,nat]
    occupied <- newPredWithType "occupied" [player,nat,nat]

    -- Pick a free tile and make it occupied by the player
    x <- newVar nat
    y <- newVar nat
    p <- newVar player
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
