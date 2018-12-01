{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}

import           ClassPath.ClassFileParser

main :: IO ()
main = do
  putStrLn ""
  cl <- loadClassFromFile "/Users/kiva/Documents/CLionProjects/KivaVM/java-out/com/imkiva/kivm/ChineseTest.class"
  putStrLn $ unpackClassName $ className cl
  putStrLn $ prettyClass cl
