import Game
import Control.Monad.State

main :: IO ()
main = runGame rockPaperScissor "game.cep"


-- Currently based on "rockPaperScissor.cep"
-- TODO base on "rockPaperScissor2.cep"
rockPaperScissor :: M ()
rockPaperScissor = do

    players 2 -- TODO return playertype to be used later
    -- playerType <- players 2
    -- TODO make "player" typesafe
    rock <- newPred "player" "rock"
    paper <- newPred "player" "paper"
    scissor <- newPred "player" "scissor"

    -- add $ predicates ["player"] ["rock","paper","scissor"]

    add $ moves "game" [rock,paper,scissor]
    add $ winCondition "result"
          [rock    `beats` scissor
          ,paper   `beats` rock
          ,scissor `beats` paper
          ,rock    `draws` rock
          ,paper   `draws` paper
          ,scissor `draws` scissor]

    -- TODO  make these nicer  
    add $ generateTurn ["jennie", "peter"]
    add $ trace "game"

     -- TODO "game" must be specificed in both moves and trace



ticTacToe :: M ()
ticTacToe = do
    numbers
    players 2
    -- TODO StageInteractive play
    -- add $ moves "play" [freeCell]
    board 3 3
    add $ winCondition "win"
        [ inARow 3
        , inAColumn 3
        , inADiagonal 3
        ]
    -- TODO go/play

    -- TODO go/draw
    -- TODO stage draw

    -- TODO context allFree
    -- TODO context init
    end
