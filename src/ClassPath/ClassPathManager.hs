module ClassPath.ClassPathManager
  ( searchClassPath
  ) where

import           ClassPath.Base
import qualified Data.List.Split    as Split
import qualified System.Directory   as Dir
import           System.Environment
import qualified System.Info        as SystemInfo

searchClassPath :: JavaClassName -> IO (Either String FilePath)
searchClassPath javaName = do
  classPathList <- getClassPathEnv
  searchClassInPath javaName classPathList

searchClassInPath :: JavaClassName -> [FilePath] -> IO (Either String FilePath)
searchClassInPath javaName (p:ps) = do
  res <- Dir.doesFileExist possible
  if res
    then return $ Right possible
    else searchClassInPath javaName ps
  where
    possible = makeClassFileName (p ++ "/") javaName -- TODO: replace hardcoded "/"
searchClassInPath javaName [] = return $ Left ("Class " ++ unpackClassName javaName ++ " not found in classpath")

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
    Just classPath -> return $ Split.splitOn getEnvSeparator classPath
    _              -> return []
