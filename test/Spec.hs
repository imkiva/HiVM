{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}

import qualified ClassPath.ClassFileParser  as CP
import qualified ClassPath.ControlFlowGraph as CFG
import           Control.Monad
import           System.Directory
import           System.FilePath

classPath :: String
classPath = "/Users/kiva/Documents/CLionProjects/KivaVM/java-out/com/imkiva/kivm"

getInst :: CP.JavaMethod -> CFG.ControlFlowGraph
getInst = CP.codeControlFlow . CP.methodBody

prettyMainMethod :: CP.JavaClass -> String
prettyMainMethod cl =
  case method of
    Just m  -> prefix ++ methodId m ++ "\n" ++ prettyCode m ++ suffix
    Nothing -> ""
  where
    method = CP.lookupMethod cl (CP.makeMethodId "main" "([Ljava/lang/String;)V")
    className = (CP.unpackClassName . CP.className) cl
    methodId m = show $ CP.methodId m
    prettyCode m = CFG.prettyControlFlowGraph $ getInst m
    prefix = "In class " ++ className ++ ": "
    suffix = "\n\n"

testFiles :: IO [FilePath]
testFiles = do
  fs <- getDirectoryContents classPath
  return $ map (classPath </>) (filter isClass fs)
  where
    isClass = (".class" ==) . takeExtension

load :: [FilePath] -> [IO String]
load = map (fmap prettyMainMethod . CP.loadClassFromFile)

main :: IO ()
main = do
  putStrLn "\n\n"
  fs <- testFiles
  forM_
    (load fs)
    (\io -> do
       str <- io
       putStrLn str)
