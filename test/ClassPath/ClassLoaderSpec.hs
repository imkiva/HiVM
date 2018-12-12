module ClassPath.ClassLoaderSpec
  ( testClassLoader
  , testClassLoaderMonad
  ) where

import           ClassPath.ClassFile
import           ClassPath.ClassLoader
import           Control.Monad.Except
import           Control.Monad.State
import           State.JavaEntrance
import           State.JavaVM

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
  let name = packClassName "com.imkiva.kivm.Main"
  result <- loadClassM name
  cl <- get
  liftIO $ printResult name cl result

runStateExceptT :: Monad m => s -> ExceptT e (StateT s m) a -> m (Either e a, s)
runStateExceptT s = flip runStateT s . runExceptT


testClassLoaderMonad :: IO ()
testClassLoaderMonad = do
  _ <- runStateT loadTestClasses makeBootstrapClassLoader
  return ()

loadClassUsingJavaContext :: JavaContext ()
loadClassUsingJavaContext = do
  _ <- loadClassJ (packClassName "com.imkiva.kivm.Main")
  _ <- loadClassJ (packClassName "com.imkiva.kivm.ChineseTest")
  _ <- loadClassJ (packClassName "com.imkiva.kivm.Polymorphism")
  jvm <- getJavaVMM
  let classLoader = getBootstrapClassLoader jvm
  liftIO $ print classLoader
  return ()
