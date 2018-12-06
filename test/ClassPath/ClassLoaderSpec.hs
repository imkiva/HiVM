module ClassPath.ClassLoaderSpec
  ( testClassLoader
  ) where

import           ClassPath.ClassFile
import           ClassPath.ClassLoader

testClassLoader :: IO ()
testClassLoader = do
  result <- loadClass makeBootstrapClassLoader (packClassName "com.imkiva.kivm.Main")
  putStrLn $
    case result of
      Right (cl, clazz) -> prettyClass clazz ++ "\n\nLoaded with ClassLoader:\n" ++ show cl
      Left err -> "unable to load class com.imkiva.kivm.Main: " ++ err
