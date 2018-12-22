module Runtime.Interpreter
  ( dispatch
  ) where

import           ClassPath.Base
import           Control.Exception    (throw)
import           Control.Monad.Except
import           Control.Monad.State  (get)
import           Data.Array.IO        (getBounds, readArray, writeArray)
import qualified Data.Binary.IEEE754  as Binary
import           Data.IORef           (readIORef)
import           State.JavaVM

toIndex :: LocalVariableIndex -> Int
toIndex = fromIntegral

-- | Type checkers
refType :: JavaValue -> JavaContext a
refType (JRefValue _) = undefined
refType _             = throwError "typeChecker: JRefType is required"

boolByteType :: JavaValue -> JavaContext a
boolByteType (JByteValue _) = undefined
boolByteType (JBoolValue _) = undefined
boolByteType _ = throwError "typeChecker: JBoolType or JByteType is required"

charType :: JavaValue -> JavaContext a
charType (JCharValue _) = undefined
charType _              = throwError "typeChecker: JCharType is required"

doubleType :: JavaValue -> JavaContext a
doubleType (JDoubleValue _) = undefined
doubleType _ = throwError "typeChecker: JDoubleType is required"

floatType :: JavaValue -> JavaContext a
floatType (JFloatValue _) = undefined
floatType _               = throwError "typeChecker: JFloatType is required"

intType :: JavaValue -> JavaContext a
intType (JIntValue _) = undefined
intType _             = throwError "typeChecker: JIntType is required"

longType :: JavaValue -> JavaContext a
longType (JLongValue _) = undefined
longType _              = throwError "typeChecker: JLongType is required"

shortType :: JavaValue -> JavaContext a
shortType (JIntValue _) = undefined
shortType _             = throwError "typeChecker: JShortType is required"

-- | Auxiliary functions
arrayLoad :: (JavaValue -> JavaContext a) -> JavaContext (Bool, JavaValue)
arrayLoad typeChecker = do
  (JIntValue index) <- popOperand
  (JRefValue arrayRef) <- popOperand
  element <-
    liftIO $ do
      (JArrayValue array) <- readIORef arrayRef
      readArray array index
  let _ = typeChecker element
  pushOperand element
  nextPC
  return (True, JNullValue)

arrayStore :: (JavaValue -> JavaContext a) -> JavaContext (Bool, JavaValue)
arrayStore typeChecker = do
  element <- popOperand
  let _ = typeChecker element
  (JIntValue index) <- popOperand
  (JRefValue arrayRef) <- popOperand
  liftIO $ do
    (JArrayValue array) <- readIORef arrayRef
    writeArray array index element
  nextPC
  return (True, JNullValue)

-- | Decode-Dispatch Interpreter
dispatch :: Instruction -> JavaContext (Bool, JavaValue)
-- |
dispatch InstructionError = throwError "Unknown instruction"
-- |
dispatch Nop = nextPC >> return (True, JNullValue)
-- | array load
dispatch Aaload = arrayLoad refType
dispatch Baload = arrayLoad boolByteType
dispatch Caload = arrayLoad charType
dispatch Daload = arrayLoad doubleType
dispatch Faload = arrayLoad floatType
dispatch Iaload = arrayLoad intType
dispatch Laload = arrayLoad longType
dispatch Saload = arrayLoad shortType
-- | array store
dispatch Aastore = arrayStore refType
dispatch Bastore = arrayStore boolByteType
dispatch Castore = arrayStore charType
dispatch Dastore = arrayStore doubleType
dispatch Fastore = arrayStore floatType
dispatch Iastore = arrayStore intType
dispatch Lastore = arrayStore longType
dispatch Sastore = arrayStore shortType
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
dispatch (Checkcast targetType)
  -- TODO: check cast
 = do
  nextPC
  return (True, JNullValue)
-- | casts
dispatch D2i = do
  (JDoubleValue v) <- popOperand
  pushOperand $ JIntValue $ (fromInteger . toInteger . Binary.doubleToWord) v
  nextPC
  return (True, JNullValue)
dispatch D2l = do
  (JDoubleValue v) <- popOperand
  pushOperand $ JLongValue $ (toInteger . Binary.doubleToWord) v
  nextPC
  return (True, JNullValue)
dispatch D2f = do
  (JDoubleValue v) <- popOperand
  pushOperand $ JFloatValue $ (Binary.wordToFloat . fromInteger . toInteger . Binary.doubleToWord) v
  nextPC
  return (True, JNullValue)
-- | wip
dispatch _ = throwError "Work in progress"
