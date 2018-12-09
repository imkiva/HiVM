module State.JavaVM where

import           ClassPath.Base
import           ClassPath.Types
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Array.IO
import           Data.IORef
import           Utils.UniqueId

type Exception = String

type JavaContext a = ExceptT Exception (StateT JavaThread IO) a

data JavaVM = JavaVM
  { getBootstrapClassLoader :: ClassLoader
  , getSystemClassLoader    :: ClassLoader
  , getAppClassLoader       :: ClassLoader
  , getJavaThreads          :: [JavaThread]
  }

data JavaThread = JavaThread
  { getThreadId    :: UniqueId
  , getThreadStack :: [JavaFrame]
  , getJavaVM      :: JavaContext JavaVM
  }

data JavaFrame = JavaFrame
  { getFrameCurrentClass  :: JavaClass
  , getFrameSlots         :: IOArray Int JavaType
  , getFramePc            :: IORef PC
  , getFramePointer       :: IORef Int
  , getFrameCurrentMethod :: JavaMethod
  }

instance Eq JavaFrame where
  (JavaFrame _ d1 p1 t1 m1) == (JavaFrame _ d2 p2 t2 m2) = d1 == d2 && p1 == p2 && t1 == t2 && m1 == m2

instance Eq JavaThread where
  thread1 == thread2 = getThreadId thread1 == getThreadId thread2

instance Show JavaThread where
  show thread = "JavaThread #" ++ show (getThreadId thread)

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
