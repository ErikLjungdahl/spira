import Game

main :: IO ()
main = runGame rockPaperScissor "game.cep"


rockPaperScissor :: Game
rockPaperScissor
     = createType ["player"]
     & prodicates "player" ["turn","rock","paper","scissor","win","lose","token"]

     & moves "game" ["rock","paper","scissor"]
     & winCondition ["rock beats scissor"
                    ,"paper beats rock"
                    ,"scissor beats paper"
                    ,"rock draws rock"
                    ,"paper draws paper"
                    ,"scissor draws scissor"]

     & generateTurn ["jennie", "peter"]

     & Trace
