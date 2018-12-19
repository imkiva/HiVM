module Runtime.Interpreter
  ( dispatch
  ) where

import           ClassPath.Base
import           Control.Exception    (throw)
import           Control.Monad.Except
import           Control.Monad.State  (get)
import           Data.Array.IO        (getBounds, readArray, writeArray)
import           Data.IORef           (readIORef)
import           State.JavaVM

toIndex :: LocalVariableIndex -> Int
toIndex = fromIntegral

dispatch :: Instruction -> JavaContext (Bool, JavaValue)
-- |
dispatch InstructionError = throwError "Unknown instruction"
-- |
dispatch Nop = nextPC >> return (True, JNullValue)
-- |
dispatch Aaload = do
  (JIntValue index) <- popOperand
  (JRefValue arrayRef) <- popOperand
  element <-
    liftIO $ do
      (JArrayValue array) <- readIORef arrayRef
      readArray array index
  pushOperand element
  nextPC
  return (True, JNullValue)
-- |
dispatch Aastore = do
  element <- popOperand
  (JIntValue index) <- popOperand
  (JRefValue arrayRef) <- popOperand
  liftIO $ do
    (JArrayValue array) <- readIORef arrayRef
    writeArray array index element
  nextPC
  return (True, JNullValue)
-- |
dispatch AconstNull = do
  pushOperand JNullValue
  increasePC 2
  return (True, JNullValue)
-- |
dispatch (Aload local) = do
  readLocal (toIndex local) >>= pushOperand
  return (True, JNullValue)
-- |
dispatch Areturn = do
  v <- popOperand
  return (False, v)
-- |
dispatch Arraylength = do
  (JRefValue arrayRef) <- popOperand
  arrayLength <-
    liftIO $ do
      (JArrayValue a) <- readIORef arrayRef
      (0, h) <- getBounds a
      return (h + 1)
  pushOperand (JIntValue arrayLength)
  nextPC
  return (True, JNullValue)
-- |
dispatch (Astore local) = do
  v <- popOperand
  writeLocal (toIndex local) v
  increasePC 2
  return (True, JNullValue)
-- |
dispatch Athrow = do
  exceptionOop <- popOperand
  thread <- get
  traces <- getCurrentStackTrace
  throw $ JavaException thread (Left "Exception") exceptionOop traces
-- |
dispatch _ = throwError "Work in progress"
