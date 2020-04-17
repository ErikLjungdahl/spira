import Game

import Data.List

main :: IO ()
main = run chess

run :: M () -> IO ()
run g = runGame g "game.cep"

ticTacToe :: M ()
ticTacToe = do
    board <- initBoard 3 3
    let (coordType, coord) = coord_t_c board
    let pieceType = piece_t board
    let (playerPieceType, pp, free) = player_t_c_free board
    let tile = tile_p board
    tile `outputNames` ["Player","Row/Col"]

    nat <- gets numberType
    player <- gets playerType

    (playernames, stage_next_player, opp) <- players ["oskar","xena"]

    -- Pick a free tile and make it occupied by the player
    x <- newBinding nat
    y <- newBinding nat
    pos <- newBinding coordType
    p <- newBinding player
    let impl = [tile [free, pos]] -* [tile [pp [p],pos]]
    stage_play<- stage "play" True [impl] p

    -- A player wins if they have 3 occupied tiles in a row/colum/diagnal
    rules <- inALine 3 (pp [p])
    stage_win <- stage "win" False (rules) p


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

    return ()


connectFour :: M ()
connectFour = do
    --nats
    --players ["jennie","simon","erik","peter","nicke","oskar"]
    (nat,suc,zero) <- gets nats
    player <- gets playerType
    (playernames, stage_next_player, opp) <- players ["xor","oskar"]


    board <- initBoard 7 1
    let (coordType, coord) = coord_t_c board
    let pieceType = piece_t board
    let (playerPieceType, pp, free) = player_t_c_free board
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
               ] -*
               [ tile [pp [p], coord [x,y]]
               , tile [free, coord [x,yP1]]  -- Makes the tile above free
               ]
    stage_play<- stage "play" True [impl] p

    -- A player wins if they have 4 occupied tiles in a row/colum/diagnal
    rules  <- inALine 4 (pp [p])
    stage_win <- stage "win" False (rules) p

    -- After play we check win condition
    stage_play `fromStageToStage` stage_win
    -- If noone has won, we go to the next player
    stage_win `fromFailedStageToStage`stage_next_player
    -- And then the next player gets to play
    stage_next_player`fromStageToStage` stage_play

    initialStageAndPlayer stage_play (head playernames)

chess :: M ()
chess = do
    (nat,suc,zero) <- gets nats
    player <- gets playerType
    (playernames, stage_next_player, opp) <- players ["hugo","musen"]
    board <- initBoard 8 8
    let (coordType, coord) = coord_t_c board
    let piece = piece_t board
    let (playerPieceType, pnp, free) = playerPiece_t_c_free board
    let tile = tile_p board
    --tile `outputNames` ["Player","Col/Row"]


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
                ] -*
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
    stage_play <- stage "play" True impls p


    -- After play we check win condition
    stage_play `fromStageToStage` stage_play

    let player1 = last playernames
    initialStageAndPlayer stage_play (player1)

    three <- zero<+3
    addToInitialBoard (tile [pnp [player1, horse],coord [three,three]])


othello :: M ()
othello = do
    (nat,suc,zero) <- gets nats
    player <- gets playerType
    (playernames, stage_next_player, opp) <- players ["black","white"]
    -- opp `outputNames` ["_","Opponent"]

    board <- initBoard 8 8
    let (coordType, coord) = coord_t_c board
    let pieceType = piece_t board
    let (playerPieceType, pp, free) = player_t_c_free board
    let tile = tile_p board

    coord_eq <- initCoordEQ

    lastPlaced <- newPred "lastPlaced" [playerPieceType, coordType]

    let black = head playernames
    let white = last playernames

    x <- newBinding nat
    y <- newBinding nat
    output <- newBinding coordType
    p <- newBinding player
    p2 <- newBinding player

    let place (startPos:pos) =
         let middlePositions = init pos
             endPos = last pos
             in
                [ opp  [p, p2]
                , tile[free, startPos]
                ]
                ++
                map (\middlePos ->
                   makePersistent (tile [pp [p2], middlePos])
                    ) middlePositions
                ++
                [ makePersistent $ tile [pp [p], endPos]
                , coord_eq [startPos, output] -- For output
                ] -*
                [ tile       [pp [p], startPos]
                , lastPlaced [pp [p], startPos]
                ]
    let coordinates = half ++ map reverse half
            where half = concat
                         [[ take n (zip cols rows)
                          | (cols,rows) <-
                                 [ ([0..]    , repeat 0)
                                 , (repeat 0 , [0..]   )
                                 , ([0..]    , [0..]   )
                                 , (reverse [0..n-1]   , [0..n-1]  )
                                 ]
                          ] | n <- [3..8]
                         ]

--                    [[(0,0),(1,0),(2,0)]
--                    ,[(0,0),(1,1),(2,2)]
--                    ,[(0,0),(0,1),(0,2)]
--                    ,[(2,0),(1,1),(0,2)]
--                    ]
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


    let flip' (startPos:pos) =
         let middlePositions = init pos
             endPos = last pos
             in
                [                  opp        [p, p2]
                , makePersistent $ lastPlaced [pp [p ], startPos ]
                ]
                ++
                map (\middlePos ->
                                   tile       [pp [p2], middlePos]
                    ) middlePositions
                ++
                [ makePersistent $ tile       [pp [p ], endPos ]
                ] -*
                map (\middlePos ->
                                   tile       [pp [p], middlePos]
                ) middlePositions

    let impls_flip = map flip' allPossiblePositions
    stage_flip <- stage "flip" False impls_flip p


    stage_remove <- stage "remove_last_player" False [ [lastPlaced [pp [p], coord [x,y]]] -* [] ] p

    points <- newPred "points" [player, nat]

    whatever <- newBinding coordType
    whoever <- newBinding player
    xp1 <- x<+1
    stage_count <- stage "count_tiles" False [ [ tile [pp [p], whatever]
                                               , points [p, x]
                                               ] -*
                                               [ points [p, xp1] ]
                                             ]  whoever -- p and p2 don't have to match

    lt <- initLT
    win <- newPred "win" [player]
    stage_winner <- stage "winner" False [ [ points [p, x]
                                            , points [p2, y]
                                            , lt [x, y]
                                            ] -*
                                            [ win [p2]]
                                          ] whoever
    stage_draw <- initDrawStage

    stage_play `fromStageToStage` stage_flip
    stage_flip `fromStageToStage` stage_flip -- Flip as much as we can
    stage_flip `fromFailedStageToStage` stage_remove -- When we no longer can flip, we remove last_placed pred
    stage_remove `fromStageToStage` stage_next_player
    stage_next_player `fromStageToStage` stage_play

    stage_play `fromFailedStageToStage` stage_count
    stage_count `fromStageToStage` stage_count --  Keep counting
    stage_count `fromFailedStageToStage` stage_winner -- When done counting, see who the winner is
    stage_winner `fromFailedStageToStage` stage_draw -- If noone wins over the other. its a draw.

    three <- zero <+ 3
    four <- zero <+ 4
    mapM addToInitialBoard [(tile [ pp [black], coord [three,four ] ])
                           ,(tile [ pp [black], coord [four ,three] ])
                           ,(tile [ pp [white], coord [three,three] ])
                           ,(tile [ pp [white], coord [four ,four ] ])
                           ]
    mapM_ (\player -> addPredToInit (points [player, zero])) playernames

    initialStageAndPlayer stage_play black
