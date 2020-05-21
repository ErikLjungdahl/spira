module TicTacToe where
import Game

main :: IO ()
main = compileGame ticTacToe "game.cep"

ticTacToe :: M ()
ticTacToe = do
    board <- initSimpleBoard 3 3
    let (coordType, _) = coord_t_c board
    let free = free_v board
    let tile = tile_p board
    (playernames, stage_next_player, _) <- players ["oskar","xena"]
    -- Pick a free tile and make it occupied by the player
    pos <- newBinding coordType
    p   <- newBinding playerType

    stage_play <- stage "play" Interactive p
                        [ [tile [free, pos]] -@ [tile [p,pos]] ]
    -- A player wins if they have 3 occupied tiles in a row/colum/diagnal
    rules <- inALine 3 p
    stage_win <- stage "win" Noninteractive p (rules)

    -- After play we check win condition
    stage_play `fromStageToStage` stage_win
    -- If noone has won, we go to the next player
    stage_win `fromFailedStageToStage`stage_next_player
    -- And then the next player gets to play
    stage_next_player`fromStageToStage` stage_play

    initialStageAndPlayer stage_play (head playernames)
