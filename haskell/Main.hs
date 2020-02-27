import Game

main :: IO ()
main = runGame ticTacToe "game.cep"


rockPaperScissor :: Game
rockPaperScissor
     = types ["player"]
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


ticTacToe :: Game
ticTacToe
    = types ["player"]
    & types ["nat"] -- Remove later?
    & typePredicates [] ["z"] "nat" -- Remove later?
    & typePredicates ["nat"] ["s"] "nat"
    & preds ["draw"]
    & predicates ["player"] ["turn","token","win"]
    & typePredicates ["player","player"] ["opp"] "bwd"
    & predicates ["nat","nat"] ["free","restore"]
    & predicates ["player","nat","nat"] ["occupied"]
    & preds ["full","not_full_yet"]
    -- TODO StageInteractive play
    -- & moves "play" [freeCell]
    & winCondition "win"
        [ inARow 3
        , inAColumn 3
        , inADiagonal 3
        -- TODO Check/draw
        ]
    -- TODO go/play

    -- TODO go/draw
    -- TODO stage draw
    & typePredicates [] ["alice","bob"] "player"
    -- TODO these predicates should be bwds
    & predicates ["alice bob"] ["opp"]
    & predicates ["bob alice"] ["opp"]
    -- TODO context allFree
    -- TODO context init
    & trace "play"
