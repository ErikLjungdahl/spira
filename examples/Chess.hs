module Chess where

import Game

main :: IO ()
main = compileGame chess "game.cep"


chess :: M ()
chess = do
    (nat,suc,zero) <- gets nats
    player <- gets playerType
    (playernames, stage_next_player, opp) <- players ["hugo","musen"]
    board <- initBoard 8 8
    let (coordType, coord) = coord_t_c board
    let piece = piece_t board
    let (playerPieceType, pnp) = playerPiece_t_c board
    let free = free_v board
    let tile = tile_p board


    horse <- newEmptyConstructor "horse" piece

    pnp_neq <- initPlayerAndPieceNotEQ opp

    --let free = head playernames
    x <- newBinding nat
    y <- newBinding nat
    p <- newBinding player
    p2 <- newBinding player
    whatever <- newBinding playerPieceType

    -- Horses move.
    let horseImpl [posB,posA] =
                [ pnp_neq [pnp [p,horse], whatever]
                , tile [pnp [p ,horse], posB]
                , tile [whatever, posA]
                ] -@
                [ tile [pnp [p,horse], posA]
                , tile [free, posB]
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
              return $ coord [a,b]
             ) cds
        ) coordinates
    let impls = map horseImpl appliedBindings
    stage_play <- stage "play" Interactive p impls


    -- After play we check win condition
    stage_play `fromStageToStage` stage_play

    let player1 = last playernames
    initialStageAndPlayer stage_play (player1)

    three <- zero<+3
    addToInitialBoard (tile [pnp [player1, horse],coord [three,three]])
