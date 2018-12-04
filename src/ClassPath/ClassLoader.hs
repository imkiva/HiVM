module ClassPath.ClassLoader
  ( ClassDictionary
  , ClassLoader(..)
  , ClassId(..)
  , makeBootstrapClassLoader
  , makeUserClassLoader
  , makeSystemClassLoader
  , lookupClass
  , saveClass
  , removeClass
  , loadClass
  , JavaClassName
  , JavaClass
  , packClassName
  , unpackClassName
  ) where

import           ClassPath.Base
import           ClassPath.ClassFile        (JavaClass)
import qualified ClassPath.ClassFile        as ClassFile
import           ClassPath.ClassPathManager
import           Data.Hashable
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Maybe
import           Prelude                    hiding (id)
import           System.IO.Unsafe
import           Utils.UniqueId

newtype ClassId =
  ClassId JavaClassName
  deriving (Eq, Ord, Show)

type ClassDictionary = HashMap ClassId JavaClass

instance Hashable ClassId where
  hashWithSalt salt (ClassId name) = hashWithSalt salt (unpackClassName name)

lookupClass :: ClassDictionary -> ClassId -> Maybe JavaClass
lookupClass dict classId = HashMap.lookup classId dict

saveClass :: ClassDictionary -> ClassId -> JavaClass -> ClassDictionary
saveClass dict classId javaClass = HashMap.insert classId javaClass dict

removeClass :: ClassDictionary -> ClassId -> ClassDictionary
removeClass dict classId = HashMap.delete classId dict

data ClassLoaderType
  = BootstrapClassLoader
  | SystemClassLoader
  | UserClassLoader
  deriving (Eq, Ord, Show)

data ClassLoader = ClassLoader
  { getLoaderType    :: ClassLoaderType
  , getLoaderId      :: UniqueId
  , getLoadedClasses :: ClassDictionary
  } deriving (Show)

instance Eq ClassLoader where
  (ClassLoader typeL idL _) == (ClassLoader typeR idR _) = typeL == typeR && idL == idR

makeClassLoader :: ClassLoaderType -> ClassLoader
makeClassLoader loaderType = ClassLoader loaderType makeUniqueId HashMap.empty

makeBootstrapClassLoader :: ClassLoader
makeBootstrapClassLoader = makeClassLoader BootstrapClassLoader

makeSystemClassLoader :: ClassLoader
makeSystemClassLoader = makeClassLoader SystemClassLoader

makeUserClassLoader :: ClassLoader
makeUserClassLoader = makeClassLoader UserClassLoader

{-# NOINLINE loadClassUnsafe #-}
loadClassUnsafe :: FilePath -> JavaClass
loadClassUnsafe = unsafePerformIO . ClassFile.loadClassFromFile

loadClass :: ClassLoader -> JavaClassName -> Maybe (ClassLoader, JavaClass)
loadClass cl@(ClassLoader clType id classes) name =
  case lookupClass classes classId of
    Just clazz -> Just (cl, clazz)
    Nothing -> Just (newClassLoader, newClazz)
      where newClazz = loadClassUnsafe (searchClassPath name)
            newClassLoader = ClassLoader clType id $ saveClass classes classId newClazz
  where
    classId = ClassId name
    searchClassPath = undefined -- placeholder
