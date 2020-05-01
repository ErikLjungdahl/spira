module ConnectFour where

import Game

main :: IO ()
main = runGame connectFour "game.cep"


connectFour :: M ()
connectFour = do
    (nat,suc,zero) <- gets nats
    player <- gets playerType
    (playernames, stage_next_player, opp) <- players ["xor","oskar"]


    board <- initSimpleBoard 7 1
    let (coordType, coord) = coord_t_c board
    let free = free_v board
    let tile = tile_p board

    lt <- initLT
    maxFact  <- newFactConstructor "max" [nat]
    six <- zero<+6 -- applyVarTimes s zero 6
    emitFact $ maxFact [six]

    -- Pick a free tile and make it occupied by the player
    x <- newBinding nat
    y <- newBinding nat
    p <- newBinding player
    m <- newBinding nat
    yP1 <- y<+1
    let impl = [ tile [free, coord [x,y]]
               , maxFact [m]
               , lt [y, m]
               ] -@
               [ tile [p, coord [x,y]]
               , tile [free, coord [x,yP1]]  -- Makes the tile above free
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
