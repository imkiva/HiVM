{-# LANGUAGE RecordWildCards #-}

module State.JavaVM where

import           ClassPath.Base
import           ClassPath.ControlFlowGraph
import           ClassPath.Types
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Array.IO
import           Data.IORef
import           Data.List                  (foldl')
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Typeable
import           Data.Word                  (Word16)

type ErrorMessage = String

type JavaContext a = ExceptT ErrorMessage (StateT JavaThread IO) a

data JavaVM = JavaVM
  { getVmClassLoader :: ClassLoader
  , getJavaThreads   :: [JavaThread]
  }

data JavaThread = JavaThread
  { getThreadId    :: ThreadId
  , getThreadStack :: [JavaFrame]
  , getThreadEnv   :: TVar JavaEnv
  , getJavaVM      :: JavaContext JavaVM
  }

data JavaFrame = JavaFrame
  { getFrameCurrentClass  :: JavaClass
  , getFrameSlots         :: IOArray Int JavaValue
  , getFramePc            :: IORef PC
  , getFramePointer       :: IORef Int
  , getFrameCurrentMethod :: JavaMethod
  }

data JavaInitArgs = JavaInitArgs
  { javaClassPath :: String
  , javaOptions   :: [(String, String)]
  , initMainClass :: JavaClassName
  , initMainArgs  :: [String]
  }

newtype JavaScope =
  JavaScope JavaClassName
  deriving (Show, Eq, Ord)

newtype JavaEnv = JavaEnv
  { getScope :: M.Map JavaScope JavaClassPool
  }

type JavaClassPool = M.Map JavaClassName JavaClassOop

data JavaClassOop = JavaClassOop
  { getOopClass        :: JavaClass
  , getOopClassLoader  :: ClassLoader
  , getOopStaticFields :: M.Map FieldId (IORef JavaValue)
  }

data JavaOop = JavaOop
  { getInstanceOopFields :: M.Map String (M.Map (String, String) (IORef JavaValue))
  , getInstanceOopMutex :: MVar Bool
  , getInstanceOopClass :: JavaClass
  }

data JavaException = JavaException
  { getExceptionThread :: JavaThread
  , getRawException    :: Either String SomeException
  , getExceptionType   :: JavaValue
  , getExceptionStack  :: [String]
  } deriving (Typeable)

data JavaValue
  = JNullValue
  | JByteValue Int
  | JCharValue Char
  | JShortValue Int
  | JBoolValue Bool
  | JIntValue Int
  | JLongValue Integer
  | JFloatValue Float
  | JDoubleValue Double
  | JObjectValue JavaOop
  | JArrayValue (IOArray Int JavaValue)
  | JRefValue (IORef JavaValue)
  deriving (Eq)

instance Show JavaException where
  show JavaException {..} = show getRawException ++ foldl' (\acc s -> acc ++ show s ++ "\n") "" getExceptionStack

instance Exception JavaException

instance Eq JavaFrame where
  (JavaFrame _ d1 p1 t1 m1) == (JavaFrame _ d2 p2 t2 m2) = d1 == d2 && p1 == p2 && t1 == t2 && m1 == m2

instance Eq JavaThread where
  thread1 == thread2 = getThreadId thread1 == getThreadId thread2

instance Eq JavaOop where
  _ == _ = error "// TODO instance Eq JavaOop"

instance Show JavaThread where
  show thread = "JavaThread #" ++ show (getThreadId thread)

----------------------------------------------------------------------
getJavaEnvRef :: JavaContext (TVar JavaEnv)
getJavaEnvRef = gets getThreadEnv

getJVMEnv :: JavaContext JavaEnv
getJVMEnv = getJavaEnvRef >>= (liftIO . atomically . readTVar)

getStack :: JavaContext [JavaFrame]
getStack = gets getThreadStack

putStack :: [JavaFrame] -> JavaContext ()
putStack st = get >>= (\e -> put $ e {getThreadStack = st})

getTopFrame :: JavaContext JavaFrame
getTopFrame = getStack >>= (\(f:_) -> return f)

popFrame :: JavaContext ()
popFrame = getStack >>= (\(_:st) -> putStack st)

readLocal :: Int -> JavaContext JavaValue
readLocal i = getTopFrame >>= (liftIO . flip readArray i . getFrameSlots)

writeLocal :: Int -> JavaValue -> JavaContext ()
writeLocal i o = getTopFrame >>= (\top -> liftIO $ writeArray (getFrameSlots top) i o)

getPC :: JavaContext PC
getPC = getTopFrame >>= (liftIO . readIORef . getFramePc)

putPC :: PC -> JavaContext ()
putPC fpc = getTopFrame >>= (liftIO . flip writeIORef fpc . getFramePc)

increasePC :: PC -> JavaContext ()
increasePC n = getPC >>= (\fpc -> putPC $ fpc + n)

nextPC :: JavaContext ()
nextPC = increasePC 1

getFP :: JavaContext Int
getFP = getTopFrame >>= (liftIO . readIORef . getFramePointer)

putFP :: Int -> JavaContext ()
putFP ffp = getTopFrame >>= (liftIO . flip writeIORef ffp . getFramePointer)

increaseFP :: Int -> JavaContext ()
increaseFP n = getFP >>= (\fp -> putFP $ fp + n)

pushOperand :: JavaValue -> JavaContext ()
pushOperand o = getFP >>= (`writeLocal` o) >> increaseFP 1

popOperand :: JavaContext JavaValue
popOperand = increaseFP (-1) >> getFP >>= readLocal

getCurrentClass :: JavaContext JavaClass
getCurrentClass = getFrameCurrentClass <$> getTopFrame

getCurrentMethod :: JavaContext JavaMethod
getCurrentMethod = getFrameCurrentMethod <$> getTopFrame

getCurrentCodeInfo :: JavaContext ControlFlowGraph
getCurrentCodeInfo = codeControlFlow . methodBody <$> getCurrentMethod

getCurrentMethodAttribute :: String -> JavaContext Attribute
getCurrentMethodAttribute attrName = do
  attrs <- methodAttributes <$> getCurrentMethod
  let Just attr = M.lookup attrName attrs
  return attr

getCurrentStackTrace :: JavaContext [String]
getCurrentStackTrace = do
  thread <- get
  let stackList = getThreadStack thread
  return (printStackTrace stackList)

printStackTrace :: [JavaFrame] -> [String]
printStackTrace = map prettyJavaFrame

prettyJavaFrame :: JavaFrame -> String
prettyJavaFrame frame = unwords [clsName, ".", methodName, "(", classSource, ")"]
  where
    method = getFrameCurrentMethod frame
    mid = methodId method
    cls = getFrameCurrentClass frame
    methodName = methodIdName mid
    clsName = unpackClassName (className cls)
    classSource = fromMaybe "Unknwon Source" (classSourceFile cls)

getInstruction :: JavaContext Instruction
getInstruction = do
  maybeCode <- cfgInstByPC <$> getCurrentCodeInfo <*> getPC
  return $ fromMaybe InstructionError maybeCode

----------------------------------------------------------------------
setVmClassLoader :: JavaVM -> ClassLoader -> JavaVM
setVmClassLoader vm cl = vm {getVmClassLoader = cl}

getJavaVMM :: JavaContext JavaVM
getJavaVMM = getJavaVM =<< get

setClassLoaderM :: ClassLoader -> JavaContext ()
setClassLoaderM cl = do
  currentVM <- getJavaVMM
  let newVM = setVmClassLoader currentVM cl
  currentThread <- get
  let newThread =
        JavaThread
          (getThreadId currentThread)
          (getThreadStack currentThread)
          (getThreadEnv currentThread)
          (return newVM)
  put newThread
