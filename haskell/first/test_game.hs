import Game


main :: IO ()
main = do
    -- variables stuff
    let preds = predicates ["turn"
                           ,"rock"
                           ,"paper"
                           ,"scissors"
                           ,"win"
                           ,"lose"
                           ,"token"]
    write preds "game.cep"

    -- Variable stuff
    let prods = prodicates "player" ["turn"
                                         ,"rock"
                                         ,"paper"
                                         ,"scissors"
                                         ,"win"
                                         ,"lose"
                                         ,"token"]
    write prods "game.cep"

    let move = moves "game" ["rock","paper","scissor"]
    write move "game.cep"

    -- 


