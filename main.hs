import Data.List
import Control.Concurrent
import Control.Monad.Trans
import Control.Monad.Trans.State
import System.Random (randomRIO)

data Cell = Live | Dead deriving (Eq)
type Board = [[Cell]]
-- initBoard =
--   [[Dead, Dead, Dead, Dead, Dead],
--   [Dead, Live, Dead, Live, Dead],
--   [Dead, Dead, Live, Dead, Dead],
--   [Dead, Dead, Dead, Dead, Dead],
--   [Dead, Dead, Dead, Dead, Dead]]

instance Show Cell where
  show Live = "◼ "
  show Dead = ". "

type Coords = (Int, Int)

getRandomCell :: IO Cell
getRandomCell = do
  r <- randomRIO (0,1)
  return $ [Live, Dead] !! r

tmp :: [Cell] -> Int -> IO [Cell]
tmp arr n = do
  c <- getRandomCell
  if n == 0 then return arr
  else tmp (c:arr) (n-1)

tmp2 :: Board -> Int -> Int -> IO Board
tmp2 arr n size = do
  c <- tmp [] size
  if n == 0 then return arr
  else tmp2 (c:arr) (n-1) size

createRandomBoard :: Int -> IO Board
createRandomBoard size = tmp2 [] size size

drawBoard :: Board -> IO ()
drawBoard board = putStrLn $ foldl (\a row ->  a ++
  (foldl (\a2 elem -> a2 ++ (show elem)) "" row ) ++ "\n") "" board

getNeighbors :: Coords -> [Coords]
getNeighbors (x, y) = [(x-1, y+1), (x, y+1), (x+1, y+1),
                      (x-1, y), (x+1, y),
                      (x-1, y-1), (x, y-1), (x+1, y-1)]

getCellState :: Board -> Coords -> Cell
getCellState board (x,y) = (board !! x) !! y

evaluateCell :: Board -> Coords -> Cell
evaluateCell board coords = do
  let (x, y) = coords
  let state = getCellState board coords
  if (x == 0 || y == 0 || x == (length board) - 1 || y == (length board) - 1) then
    Dead
  else do
    let neighbors = getNeighbors coords
    let neighborsStates = map (getCellState board) neighbors
    let liveNeighborsCount = length $ filter (==Live) neighborsStates
    if state == Dead then do
      if liveNeighborsCount == 3 then Live
      else state
    else do
      if liveNeighborsCount == 2 || liveNeighborsCount == 3 then state
      else Dead

getBoardCoords :: Board -> [[Coords]]
getBoardCoords board = map (\(i, row) -> map (\(j, _) -> (i,j))
                                          $ zip [0..] row) $ zip [0..] board

evaluateBoard :: Board -> Board
evaluateBoard board = do
  map (\row -> map (evaluateCell board) row) $ getBoardCoords board


gameOfLife :: StateT Board IO ()
gameOfLife = do
  prevBoard <- get
  liftIO $ putStrLn "\ESC[2J"
  liftIO $ drawBoard prevBoard
  put $ evaluateBoard prevBoard
  liftIO $ threadDelay 200000
  gameOfLife

main = do
  initBoard <- createRandomBoard 20
  runStateT gameOfLife initBoard