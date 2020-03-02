import Game
import Control.Monad.State

main :: IO ()
main = runGame ticTacToe "game.cep"


rockPaperScissor :: Game
rockPaperScissor
     =
--     do
--         player  <- newPlayer "player"
--         scissor <- newPred player "scissor"
        types ["player"]
     -- TODO extract the predicates from the moves/wincondition?
     & predicates ["player"] ["turn","rock","paper","scissor","win","lose","token"]
     & moves "game" ["rock","paper","scissor"]
     & winCondition "result"
         ["rock"    `beats` "scissor"
         ,"paper"   `beats` "rock"
         ,"scissor" `beats` "paper"
         ,"rock"    `draws` "rock"
         ,"paper"   `draws` "paper"
         ,"scissor" `draws` "scissor"]

     & generateTurn ["jennie", "peter"]
     & trace "game"

     -- TODO "game" must be specificed in both moves and trace



ticTacToe :: M ()
ticTacToe = do
    numbers
    players 2
    -- TODO StageInteractive play
    -- add $ moves "play" [freeCell]
    board 3
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
