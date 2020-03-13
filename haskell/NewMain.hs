import NewGame
import Control.Monad.State

main :: IO ()
main = runGame ticTacToe "game.cep"

ticTacToe :: M ()
ticTacToe = do
    --nats
    players ["jennie","simon","erik","peter","nicke","oskar"]
    return ()