module ConnectFour where

import Game

main :: IO ()
main = compileGame connectFour "game.cep"


connectFour :: M ()
connectFour = do
    (playernames, stage_next_player, _) <- players ["xor","oskar"]


    board <- initSimpleBoard 7 1
    let (_, coord) = coord_t_c board
    let free = free_v board
    let tile = tile_p board

    lt <- initLT
    maxFact  <- newFactConstructor "max" [nat]
    emitFact $ maxFact [zero<+6]

    -- Pick a free tile and make it occupied by the player
    x <- newBinding nat
    y <- newBinding nat
    p <- newBinding playerType
    m <- newBinding nat
    let impl = [ tile [free, coord [x,y]]
               , maxFact [m]
               , lt [y, m]
               ] -@
               [ tile [p, coord [x,y]]
               , tile [free, coord [x,y<+1]]  -- Makes the tile above free
               ]
    stage_play<- stage "play" Interactive p [impl]

    -- A player wins if they have 4 occupied tiles in a row/colum/diagnal
    rules  <- inALine 4 (p)
    stage_win <- stage "win" Noninteractive p (rules)

    -- After play we check win condition
    stage_play `fromStageToStage` stage_win
    -- If noone has won, we go to the next player
    stage_win `fromFailedStageToStage`stage_next_player
    -- And then the next player gets to play
    stage_next_player`fromStageToStage` stage_play

    initialStageAndPlayer stage_play (head playernames)
