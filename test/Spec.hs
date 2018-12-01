{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}

import           ClassPath.ClassFileParser

main :: IO ()
main = do
  cl <- loadClass "/Users/kiva/Documents/CLionProjects/KivaVM/java-out/com/imkiva/kivm/ChineseTest.class"
  putStrLn $ prettyClass cl
