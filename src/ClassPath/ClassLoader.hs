module ClassPath.ClassLoader
  ( ClassDictionary
  , ClassLoader(..)
  , ClassId(..)
  , makeClassLoader
  , lookupClass
  , saveClass
  , removeClass
  , loadClass
  , loadClassM
  , loadClassJ
  , JavaClassName
  , JavaClass
  , packClassName
  , unpackClassName
  ) where

import           ClassPath.Base
import           ClassPath.ClassFile        (JavaClass)
import qualified ClassPath.ClassFile        as ClassFile
import           ClassPath.ClassPathManager
import           ClassPath.Types
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.HashMap.Strict        as HashMap
import           Data.Maybe
import           Prelude                    hiding (id)
import           State.JavaVM
import           Utils.UniqueId

lookupClass :: ClassDictionary -> ClassId -> Maybe JavaClass
lookupClass dict classId = HashMap.lookup classId dict

saveClass :: ClassDictionary -> ClassId -> JavaClass -> ClassDictionary
saveClass dict classId javaClass = HashMap.insert classId javaClass dict

removeClass :: ClassDictionary -> ClassId -> ClassDictionary
removeClass dict classId = HashMap.delete classId dict

makeClassLoader :: ClassLoader
makeClassLoader = ClassLoader makeUniqueId HashMap.empty

loadNewClass :: JavaClassName -> IO (Either String JavaClass)
loadNewClass javaName = do
  maybeFile <- searchClassPath javaName
  case maybeFile of
    Right file -> do
      clazz <- ClassFile.loadClassFromFile file
      return $ Right clazz
    Left err -> return $ Left err

loadClass :: ClassLoader -> JavaClassName -> IO (Either String (ClassLoader, JavaClass))
loadClass cl@(ClassLoader id classes) name =
  case lookupClass classes classId of
    Just clazz -> return $ Right (cl, clazz)
    Nothing -> do
      newClazz <- loadNewClass name
      return $ do
        clazz <- newClazz
        let newClassLoader = ClassLoader id (saveClass classes classId clazz)
        return (newClassLoader, clazz)
  where
    classId = ClassId name

loadClassM :: JavaClassName -> StateT ClassLoader IO (Either String JavaClass)
loadClassM javaName = do
  cl <- get
  result <- liftIO =<< loadClass cl <$> return javaName
  case result of
    Right (newCl, clazz) -> do
      put newCl
      return $ Right clazz
    Left err -> return $ Left err

loadClassJ :: JavaClassName -> JavaContext JavaClass
loadClassJ javaName = do
  vm <- getJavaVMM
  let cl = getVmClassLoader vm
  result <- liftIO =<< loadClass cl <$> return javaName
  case result of
    Right (newCl, clazz) -> do
      setClassLoaderM newCl
      return clazz
    Left err -> throwError err
