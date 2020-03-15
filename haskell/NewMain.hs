import NewGame
import Control.Monad.State

main :: IO ()
main = runGame ticTacToe "game.cep"

ticTacToe :: M ()
ticTacToe = do
    --nats
    --players ["jennie","simon","erik","peter","nicke","oskar"]
    (nat,s,z) <- gets nats
    player <- players ["simon","jennie","erik"]

    free <- newPredWithType "free" [nat,nat]
    occupied <- newPredWithType "occupied" [player,nat,nat]


    x <- newVar nat
    y <- newVar nat
    p <- newVar player
    let impl = [applyPred free [x,y]] -* [applyPred occupied [p,x,y]]
    stage_play<- stage "play" True [impl] p

    rowrule <- inARow 3 occupied p
    stage_win <- stage "win" False [rowrule] p

    transition "play_to_win" (fromStageToStageWith stage_play stage_win p)
    transition "win_to_play" (fromFailedStageToStageWith stage_win stage_play p)

    return ()
