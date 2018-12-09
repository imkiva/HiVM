module State.JavaVM where

import           ClassPath.Base
import           ClassPath.ClassLoader
import           ClassPath.Types
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Array             (Array)
import qualified Data.Array             as Array
import           Data.Array.IO
import           Data.Array.MArray
import           Data.IORef
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Utils.UniqueId

type Exception = String

type JavaContext a = ExceptT Exception (StateT JavaThread IO) a

data JavaVM = JavaVM
  { getJavaClassLoader :: ClassLoader
  , getJavaThreads     :: [JavaThread]
  } deriving (Show)

data JavaThread = JavaThread
  { getThreadId      :: UniqueId
  , getThreadStack   :: [JavaFrame]
  , getThreadContext :: JavaContext JavaVM
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
