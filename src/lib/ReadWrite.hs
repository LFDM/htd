module ReadWrite (
  createNewTodoFile
) where

import System.Directory
import System.FilePath
import System.Environment as Env

todoFileName = ".htd.yaml"

createNewTodoFile :: IO Bool
createNewTodoFile = do
  cwd <- getCurrentDirectory
  let f = joinPath [cwd, todoFileName]
  doesFileExist f >>= tryCreateNewTodoFileAtPath f

tryCreateNewTodoFileAtPath :: FilePath -> Bool -> IO Bool
tryCreateNewTodoFileAtPath p True  = return False
tryCreateNewTodoFileAtPath p False = writeFile p "" >> return True


