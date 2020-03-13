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
    occupied <- newPredWithType "occupied" [player,nat,nat]
    rowrule <- inARow 3 occupied
    stage "win" False [rowrule]
    return ()
