import Game

main :: IO ()
main = do
    -- Variable stuff
    let t = createType ["player"]
    write t "game.cep"

    let prods = prodicates "player" ["turn"
                                    ,"rock"
                                    ,"paper"
                                    ,"scissor"
                                    ,"win"
                                    ,"lose"
                                    ,"token"]
    write prods "game.cep"

    let move = moves "game" ["rock","paper","scissor"]
    write move "game.cep"

    let win = winCondition ["rock beats scissor"
                           ,"paper beats rock"
                           ,"scissor beats paper"
                           ,"rock draws rock"
                           ,"paper draws paper"
                           ,"scissor draws scissor"]
    write win "game.cep"

    let players = generateTurn ["jennie", "peter"]
    write players "game.cep"

    write Trace "game.cep" 

