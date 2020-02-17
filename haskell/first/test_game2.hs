import Game


main :: IO ()
main = do
    -- variables stuff
    clearFile "game.cep"
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
    let stageGame = Stage "game" (moves ["rock","scissor","paper"])

    write stageGame "game.cep"
    --
