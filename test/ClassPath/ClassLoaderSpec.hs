module ClassPath.ClassLoaderSpec
  ( testClassLoader
  ) where

import           ClassPath.ClassFile
import           ClassPath.ClassLoader

testClassLoader :: IO ()
testClassLoader = do
  maybeClass <- loadClass makeBootstrapClassLoader (packClassName "com.imkiva.kivm.Main")
  putStrLn $
    case maybeClass of
      Just (cl, clazz) -> prettyClass clazz ++ "\n\nLoaded with ClassLoader:\n" ++ show cl
      Nothing -> "unable to load class com.imkiva.kivm.Main"
