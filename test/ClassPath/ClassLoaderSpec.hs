module ClassPath.ClassLoaderSpec
  ( testClassLoader
  , testClassLoaderMonad
  ) where

import           ClassPath.ClassFile
import           ClassPath.ClassLoader
import           Control.Monad.State

printResult :: JavaClassName -> ClassLoader -> Either String JavaClass -> IO ()
printResult _ cl (Right clazz) = putStrLn $ prettyClass clazz ++ "\n\nLoaded with ClassLoader:\n" ++ show cl
printResult javaName _ (Left err) = putStrLn $ "unable to load class " ++ unpackClassName javaName ++ ": " ++ err

testClassLoader :: IO ()
testClassLoader = do
  let cl = makeBootstrapClassLoader
  let name = packClassName "com.imkiva.kivm.Main"
  result <- loadClass cl name
  case result of
    Right (newCl, clazz) -> printResult name newCl (Right clazz)
    Left err             -> printResult name cl (Left err)

loadTestClasses :: StateT ClassLoader IO ()
loadTestClasses = do
  let name = packClassName "io.imkiva.kivm.Main"
  result <- loadClassM name
  cl <- get
  liftIO $ printResult name cl result

testClassLoaderMonad :: IO ()
testClassLoaderMonad = do
  _ <- runStateT loadTestClasses makeBootstrapClassLoader
  return ()
