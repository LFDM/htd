module Cli
  ( run
  ) where

import           Data.List
import           Data.Maybe
import           System.Environment as Env
import           System.IO          ()
import           Printer
import           ReadWrite          (createNewTodoFile, readTodoList)
import           Run

type Command = [String] -> IO ()

run :: IO ()
run = do
  (cmd:args) <- Env.getArgs
  findCommand cmd args

commandList :: [(String, Command)]
commandList = [("help", help), ("show", showTodos), ("new", newTodoFile), ("run", runApp)]

findCommand :: String -> Command
findCommand cmd = fromMaybe unknown $ lookup cmd commandList

unknown :: Command
unknown args = putStr $ nl (unwords output)
  where
    output = ["Unknown command: ", unwords args]

help :: Command
help = logCmd "help"

showTodos :: Command
showTodos [] = do
  readTodoList
  return ()

newTodoFile :: Command
newTodoFile [] = createNewTodoFile >>= notify
  where notify True = putStrLn "New todo list created!"
        notify False = putStrLn "Todo list already exists!"

logCmd :: String -> Command
logCmd cmd args = putStr $ nl (unwords output)
  where
    output = ["calling", cmd, "with", unwords args]
