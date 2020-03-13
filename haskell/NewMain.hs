import NewGame
import Control.Monad.State

main :: IO ()
main = runGame ticTacToe "game.cep"

ticTacToe :: M ()
ticTacToe = do
    (nat,s,z) <- gets nats
    player <- gets player
    occupied <- newPred "occupied" [player,nat,nat]
    rowrule <- inARow 3 occupied
    stage "win" False [rowrule]
    return ()
