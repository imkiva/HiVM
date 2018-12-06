module ClassPath.ClassPathManager
  ( searchClassPath
  ) where

import           ClassPath.Base
import           Data.List.Split
import qualified System.Directory   as Dir
import           System.Environment
import qualified System.Info        as SystemInfo

searchClassPath :: JavaClassName -> IO (Maybe FilePath)
searchClassPath javaName = do
  classPathList <- getClassPathEnv
  searchClassInPath javaName classPathList

searchClassInPath :: JavaClassName -> [FilePath] -> IO (Maybe FilePath)
searchClassInPath javaName (p:ps) = do
  res <- Dir.doesFileExist possible
  if res
    then return $ Just possible
    else searchClassInPath javaName ps
  where
    possible = makeClassFileName p javaName
searchClassInPath _ [] = return Nothing

makeClassFileName :: String -> JavaClassName -> String
makeClassFileName classPath = (++ ".class") . (classPath ++) . dotsToSlashes . unpackClassName

getEnvSeparator :: String
getEnvSeparator =
  case SystemInfo.os of
    "windows" -> ";"
    _         -> ":"

getClassPathEnv :: IO [FilePath]
getClassPathEnv = do
  env <- getEnvironment
  case lookup "CLASSPATH" env of
    Just classPath -> return $ splitOn getEnvSeparator classPath
    _              -> return []
