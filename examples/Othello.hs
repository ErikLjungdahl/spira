module Othello where

import Game

main :: IO ()
main = compileGame othello "game.cep"


othello :: M ()
othello = do
    (playernames, stage_next_player, opp) <- players ["black","white"]

    board <- initSimpleBoard 8 8
    let (coordType, coord) = coord_t_c board
    let free = free_v board
    let tile = tile_p board
    tile `outputNames` ["Turn","_/_"]

    coord_eq <- initCoordEQ

    lastPlaced <- newPredConstructor "lastPlaced" [playerType, coordType]

    let black = head playernames
    let white = last playernames

    x <- newBinding nat
    y <- newBinding nat
    output <- newBinding coordType
    p <- newBinding playerType
    p2 <- newBinding playerType

    let place (startPos:pos) =
         let middlePositions = init pos
             endPos = last pos
             in
                [ opp  [p, p2]
                , tile[free, startPos]
                ]
                ++
                map (\middlePos ->
                   makePersistent (tile [p2, middlePos])
                    ) middlePositions
                ++
                [ makePersistent $ tile [p, endPos]
                , coord_eq [startPos, output] -- For output
                ] -@
                [ tile       [p, startPos]
                , lastPlaced [p, startPos]
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

    let allPossiblePositions =
            map (\positions ->
                    map (\(a,b) -> coord [x <+ a , y <+ b]
                        ) positions
                ) coordinates

    let impls_play = map place (allPossiblePositions)
    stage_play <- stage "play" Interactive p impls_play


    let flip' (startPos:pos) =
         let middlePositions = init pos
             endPos = last pos
             in
                [                  opp        [p, p2]
                , makePersistent $ lastPlaced [p, startPos ]
                ]
                ++
                map (\middlePos ->
                                   tile       [p2, middlePos]
                    ) middlePositions
                ++
                [ makePersistent $ tile       [p, endPos ]
                ] -@
                map (\middlePos ->
                                   tile       [p, middlePos]
                ) middlePositions

    let impls_flip = map flip' allPossiblePositions
    stage_flip <- stage "flip" Noninteractive p impls_flip


    stage_remove <- stage "remove_last_player" Noninteractive p [ [lastPlaced [p, coord [x,y]]] -@ [] ]

    points <- newPredConstructor "points" [playerType, nat]

    whatever <- newBinding coordType
    whoever <- newBinding playerType
    stage_count <- stage "count_tiles" Noninteractive whoever -- p and p2 don't have to match
        [ [ tile [p, whatever]
          , points [p, x]
          ] -@
          [ points [p, x<+1] ]
        ]

    lt <- initLT
    win <- newPredConstructor "win" [playerType]
    stage_winner <- stage "winner" Noninteractive whoever
        [ [ points [p, x]
          , points [p2, y]
          , lt [x, y]
          ] -@
          [ win [p2]]
        ]
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

    let three = zero <+ 3
    let four  = zero <+ 4
    mapM_ addToInitialBoard [(tile [ black, coord [three,four ] ])
                           ,(tile [ black, coord [four ,three] ])
                           ,(tile [ white, coord [three,three] ])
                           ,(tile [ white, coord [four ,four ] ])
                           ]
    mapM_ (\player -> addPredToInit (points [player, zero])) playernames

    initialStageAndPlayer stage_play black
