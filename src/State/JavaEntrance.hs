module State.JavaEntrance
  ( runJava
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
