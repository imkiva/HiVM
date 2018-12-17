module Runtime.Interpreter
  ( dispatch
  ) where

import           ClassPath.Base
import           Control.Monad.Except
import           Data.Array.IO        (readArray, writeArray)
import           Data.IORef           (readIORef)
import           State.JavaVM

dispatch :: Instruction -> JavaContext (Bool, JavaValue)
dispatch InstructionError = throwError "Unknown instruction"

dispatch Nop = increasePC 1 >> return (True, JNullValue)

dispatch Aaload = do
  (JIntValue index) <- popOperand
  (JRefValue arrayRef) <- popOperand
  element <-
    liftIO $ do
      (JArrayValue array) <- readIORef arrayRef
      readArray array index
  pushOperand element
  increasePC 1
  return (True, JNullValue)

dispatch Aastore = do
  element <- popOperand
  (JIntValue index) <- popOperand
  (JRefValue arrayRef) <- popOperand
  liftIO $ do
    (JArrayValue array) <- readIORef arrayRef
    writeArray array index element
  increasePC 1
  return (True, JNullValue)

dispatch AconstNull = do
  pushOperand JNullValue
  increasePC 1
  return (True, JNullValue)

dispatch _ = throwError "Work in progress"
