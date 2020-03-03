import Game
import Control.Monad.State

main :: IO ()
main = runGame ticTacToe "game.cep"


rockPaperScissor :: M ()
rockPaperScissor = do
--     do
--         player  <- newPlayer "player"
--         scissor <- newPred player "scissor"
    players 2

    -- TODO extract the predicates from the moves/wincondition?
    add $ predicates ["player"] ["rock","paper","scissor"]
    
    add $ moves "game" ["rock","paper","scissor"]
    add $ winCondition "result"
          ["rock"    `beats` "scissor"
          ,"paper"   `beats` "rock"
          ,"scissor" `beats` "paper"
          ,"rock"    `draws` "rock"
          ,"paper"   `draws` "paper"
          ,"scissor" `draws` "scissor"]

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
