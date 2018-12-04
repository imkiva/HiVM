module State.JavaVM where

import           ClassPath.Base
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

type JavaVM a = ExceptT String (StateT JavaThread IO) a

newtype JavaThread = JavaThread
  { stack :: [JavaFrame]
  } deriving (Eq)

data JavaFrame = JavaFrame
  { frameCurrentClass  :: JavaClass
  , frameSlots         :: IOArray Int JavaType
  , framePc            :: IORef PC
  , framePointer       :: IORef Int
  , frameCurrentMethod :: JavaMethod
  }

instance Eq JavaFrame where
  (JavaFrame _ d1 p1 t1 m1) == (JavaFrame _ d2 p2 t2 m2) = d1 == d2 && p1 == p2 && t1 == t2 && m1 == m2
