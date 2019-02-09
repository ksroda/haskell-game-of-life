import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.Reader
import System.Random (randomRIO)
import Text.Read

get' = Control.Monad.Trans.State.get
ask' = Control.Monad.Reader.ask
lift' = Control.Monad.Trans.Class.lift

getRandom :: Int -> IO Int
getRandom v = randomRIO (1, v)

s :: Int -> String
s v = "Wybierz kubek [1-" ++ (show v) ++ "]: "

game :: ReaderT Int (StateT Int (ExceptT String IO)) Int
game = do
  currentState <- ReaderT $ return $ get'
  count <- ask'
  kub <- liftIO $ getRandom count
  liftIO $ putStrLn "Kubek wylosowany!"
  picked <- ReaderT $ return $ lift' $ ExceptT $ fmap readEither $ liftIO $ (putStr $ s count) >> getLine
  if kub == picked then do
    newState <- return $ currentState + 100
    liftIO $ putStrLn $ "Gratuluje! Masz teraz " ++ (show newState) ++ " punktów"
    ReaderT $ return $ put newState
  else do
    newState <- return $ currentState - 50
    liftIO $ putStrLn $ "Niestety nie, próbuj dalej! Masz teraz " ++ (show newState) ++ " punktów"
    ReaderT $ return $ put newState
  liftIO $ putStrLn ""
  game

main = do
  runExceptT $ runStateT (runReaderT game 3) 0
