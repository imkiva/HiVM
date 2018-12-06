module ClassPath.ClassLoaderSpec
  ( testClassLoader
  ) where

import           ClassPath.ClassFile
import           ClassPath.ClassLoader
import           Control.Monad.State
import           State.JavaVM

testClassLoader :: IO ()
testClassLoader = do
  result <- loadClass makeBootstrapClassLoader (packClassName "com.imkiva.kivm.Main")
  putStrLn $
    case result of
      Right (cl, clazz) -> prettyClass clazz ++ "\n\nLoaded with ClassLoader:\n" ++ show cl
      Left err -> "unable to load class com.imkiva.kivm.Main: " ++ err

loadClassWithState :: State ClassLoader JavaClass
loadClassWithState = state $ error "failed"

testClassLoaderMonad :: State ClassLoader JavaClass
testClassLoaderMonad = undefined
