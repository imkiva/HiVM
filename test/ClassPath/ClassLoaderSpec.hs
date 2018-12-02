module ClassPath.ClassLoaderSpec
  ( testClassLoader
  ) where

import           ClassPath.ClassFile
import           ClassPath.ClassLoader

testClassLoader :: IO ()
testClassLoader = do
  let cl = makeBootstrapClassLoader
  putStrLn $
    case loadClass cl $ packClassName "com.imkiva.kivm.Main" of
      Nothing -> "unable to load class com.imkiva.kivm.Main"
      Just (newCl, clazz) -> prettyClass clazz ++ "\nLoaded with ClassLoader:\n" ++ show newCl
