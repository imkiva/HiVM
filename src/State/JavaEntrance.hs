module State.JavaEntrance
  ( runJava
  , createJavaVM
  , createThread
  ) where

import           ClassPath.ClassLoader
import           Control.Monad.Except
import           Control.Monad.State
import           State.JavaVM
import qualified Utils.UniqueId        as UID

type ExitCode = Int

runJava :: JavaInitArgs -> IO ExitCode
runJava initArgs = do
  return 0

createJavaVM :: JavaContext JavaVM
createJavaVM = return $ JavaVM makeBootstrapClassLoader makeSystemClassLoader makeAppClassLoader []

createThread :: JavaContext JavaVM -> JavaContext JavaThread
createThread jvm = return $ JavaThread UID.makeUniqueId [] jvm
