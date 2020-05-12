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

    player <- gets playerType

    (playernames, stage_next_player, _) <- players ["oscar","xena"]

    -- Pick a free tile and make it occupied by the player
    pos <- newBinding coordType
    p <- newBinding player
    let impl = [tile [free, pos]] -@ [tile [p,pos]]
    stage_play<- stage "play" Interactive p [impl]

    -- A player wins if they have 3 occupied tiles in a row/colum/diagnal
    rules <- inALine 3 (p)
    stage_win <- stage "win" Noninteractive p (rules)


    stage_draw <- initDrawStage

    -- After play we check win condition
    stage_play `fromStageToStage` stage_win
    -- If we can't play, all tiles are filled and it is a Draw
    stage_play `fromFailedStageToStage` stage_draw
    -- If noone has won, we go to the next player
    stage_win `fromFailedStageToStage`stage_next_player
    -- And then the next player gets to play
    stage_next_player`fromStageToStage` stage_play


    initialStageAndPlayer stage_play (head playernames)
