module State.JavaEntrance where

import           Control.Monad.Except
import           Control.Monad.State
import           State.JavaVM

runJava :: JavaInitArgs -> IO Int
runJava initArgs = do
  return 0
