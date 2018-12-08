import           ClassPath.ClassFileSpec
import           ClassPath.ClassLoaderSpec

runTest :: String -> IO () -> IO ()
runTest tag io = do
  putStrLn $ ":: Running Test: " ++ tag
  io

main :: IO ()
main = do
  runTest "testClassFile" testClassFile
  runTest "testClassLoader" testClassLoader
  runTest "testClassLoader with Monad" testClassLoaderMonad
