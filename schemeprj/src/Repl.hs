module Repl where
import System.IO
import Lib
import Control.Monad
import Envir
import Global
import System.Console.Haskeline
import Data.IORef
import Control.Monad.Error

flushStr :: String -> IO()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine


evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env


evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action


runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args",List $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load",String (args !!0)])) >>= hPutStrLn stderr



runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

repl :: IO ()
repl = do
  env <- primitiveBindings
  runInputT defaultSettings (loop env) where
    loop env = do
      minput <- getInputLine "ready> "
      case minput of
        Nothing -> outputStrLn "Good luck. dude"
        Just input -> do
          liftIO $ evalAndPrint env input
          loop env
