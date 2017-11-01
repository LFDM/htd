module ReadWrite (
  createNewTodoFile
, readTodoList
) where

import Control.Lens
import Data.Maybe
import System.Directory
import System.FilePath
import System.Environment as Env
import Todos
import Todo
import qualified Data.Yaml as Y

import qualified Data.ByteString.Char8 as BS


todoFileName = ".htd.yaml"

getTodoFileName :: String -> FilePath
getTodoFileName dir = joinPath [dir, todoFileName]

createNewTodoFile :: IO Bool
createNewTodoFile = do
  cwd <- getCurrentDirectory
  let f = getTodoFileName cwd
  doesFileExist f >>= tryCreateNewTodoFileAtPath f

tryCreateNewTodoFileAtPath :: FilePath -> Bool -> IO Bool
tryCreateNewTodoFileAtPath p True  = return False
tryCreateNewTodoFileAtPath p False = writeFile p "" >> return True

readTodoList :: IO (Maybe Todos)
readTodoList = do
  ts <- getCurrentDirectory >>= findTodoFile >>= readTodoFile
  print $ fromMaybe (createTodosContainer "" []) ts
  return Nothing

findTodoFile :: String -> IO (Maybe FilePath)
findTodoFile "" = return Nothing
findTodoFile dir = doesFileExist f >>= returnOrRecurse
  where f = getTodoFileName dir
        returnOrRecurse True = return (Just f)
        returnOrRecurse False = findTodoFile $ getParentDir dir

readTodoFile :: Maybe FilePath -> IO (Maybe Todos)
readTodoFile Nothing = return Nothing
readTodoFile (Just p) = BS.readFile p >>= parse >>= createTodos
  where parse f = return (Y.decode f :: Maybe [Todo])
        createTodos Nothing = return Nothing
        createTodos (Just todos) = return $ Just (createTodosContainer p todos)

writeTodoFile :: Todos -> IO ()
writeTodoFile ts = BS.writeFile p yaml
  where yaml = Y.encode (getTodosList ts)
        p = view path ts

getParentDir :: String -> String
getParentDir = joinPath . init . splitDirectories


