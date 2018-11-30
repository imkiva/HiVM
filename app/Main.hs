module Main where

import ClassPath.ClassFileParser

main :: IO ()
main = do
  cl <- loadClass "/Users/kiva/Documents/CLionProjects/KivaVM/java-out/com/imkiva/kivm/ChineseTest.class"
  putStrLn $ showClass cl

