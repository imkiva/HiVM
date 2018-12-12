{-# LANGUAGE RecordWildCards #-}

module State.JavaVM where

import           ClassPath.Base
import           ClassPath.Types
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Array.IO
import           Data.IORef
import           Data.List              (foldl')
import qualified Data.Map               as M
import           Data.Typeable
import           Utils.UniqueId

type JavaContext a = ExceptT JavaException (StateT JavaThread IO) a

data JavaVM = JavaVM
  { getBootstrapClassLoader :: ClassLoader
  , getSystemClassLoader    :: ClassLoader
  , getAppClassLoader       :: ClassLoader
  , getJavaThreads          :: [JavaThread]
  }

data JavaThread = JavaThread
  { getThreadId    :: UniqueId
  , getThreadStack :: [JavaFrame]
  , getThreadEnv   :: TVar JavaEnv
  , getJavaVM      :: JavaContext JavaVM
  }

data JavaFrame = JavaFrame
  { getFrameCurrentClass  :: JavaClass
  , getFrameSlots         :: IOArray Int JavaType
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
  , getOopStaticFields :: M.Map FieldId (IORef JavaType)
  }

data JavaOop = JavaOop
  { getInstanceOopFields :: M.Map String (M.Map (String, String) (IORef JavaType))
  , getInstanceOopMutex :: MVar Bool
  , getInstanceOopClass :: JavaClass
  }

data JavaException = JavaException
  { getExceptionThread :: JavaThread
  , getRawException    :: Either String SomeException
  , getExceptionType   :: JavaType
  , getExceptionStack  :: [String]
  } deriving (Typeable)

instance Show JavaException where
  show JavaException {..} = show getRawException ++ foldl' (\acc s -> acc ++ show s ++ "\n") "" getExceptionStack

instance Exception JavaException

instance Eq JavaFrame where
  (JavaFrame _ d1 p1 t1 m1) == (JavaFrame _ d2 p2 t2 m2) = d1 == d2 && p1 == p2 && t1 == t2 && m1 == m2

instance Eq JavaThread where
  thread1 == thread2 = getThreadId thread1 == getThreadId thread2

instance Eq JavaOop where
  oop1 == oop2 = undefined

instance Show JavaThread where
  show thread = "JavaThread #" ++ show (getThreadId thread)

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

getClassLoaderJ :: ClassLoaderType -> JavaVM -> ClassLoader
getClassLoaderJ BootstrapClassLoader = getBootstrapClassLoader
getClassLoaderJ SystemClassLoader    = getSystemClassLoader
getClassLoaderJ AppClassLoader       = getAppClassLoader

setClassLoaderJ :: ClassLoaderType -> JavaVM -> ClassLoader -> JavaVM
setClassLoaderJ BootstrapClassLoader (JavaVM _ sc ac ts) cl = JavaVM cl sc ac ts
setClassLoaderJ SystemClassLoader (JavaVM bc _ ac ts) cl = JavaVM bc cl ac ts
setClassLoaderJ AppClassLoader (JavaVM bc sc _ ts) cl = JavaVM bc sc cl ts

getJavaVMM :: JavaContext JavaVM
getJavaVMM = getJavaVM =<< get

setClassLoaderM :: ClassLoaderType -> ClassLoader -> JavaContext ()
setClassLoaderM loaderType cl = do
  currentVM <- getJavaVMM
  let newVM = setClassLoaderJ loaderType currentVM cl
  currentThread <- get
  let newThread = JavaThread (getThreadId currentThread) (getThreadStack currentThread) (return newVM)
  put newThread
