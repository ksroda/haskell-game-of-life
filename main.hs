import Data.List
import Control.Concurrent
import Control.Monad.Trans
import Control.Monad.Trans.State

data Cell = Live | Dead | Empty
type Board = [[Cell]]
board =
  [[Empty, Empty, Empty],
  [Empty, Empty, Empty],
  [Empty, Empty, Empty]]

instance Show Cell where
  show Live = "O "
  show Dead = "X "
  show Empty = ". "

drawBoard :: IO ()
drawBoard = putStrLn $ foldl (\a row ->  a ++
  (foldl (\a2 elem -> a2 ++ (show elem)) "" row ) ++ "\n") "" board

-- evaluateNewBoard ::
-- evaluateNewBoard


gameOfLife :: StateT Board IO ()
gameOfLife = do
  prevBoard <- get
  liftIO $ putStrLn "\ESC[2J"
  liftIO $ drawBoard
  liftIO $ threadDelay 3000000
  gameOfLife

main = runStateT gameOfLife board