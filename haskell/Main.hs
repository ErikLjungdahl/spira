import Game

main :: IO ()
main = runGame rockPaperScissor "game.cep"


rockPaperScissor :: Game
rockPaperScissor
     = createType ["player"]
     -- TODO extract the predicates from the moves/wincondition?
     & prodicates "player" ["turn","rock","paper","scissor","win","lose","token"]
     & moves "game" ["rock","paper","scissor"]
     & winCondition ["rock beats scissor"
                    ,"paper beats rock"
                    ,"scissor beats paper"
                    ,"rock draws rock"
                    ,"paper draws paper"
                    ,"scissor draws scissor"]

     & generateTurn ["jennie", "peter"]
     & trace "game"

     -- TODO "game" must be specificed in both moves and trace
