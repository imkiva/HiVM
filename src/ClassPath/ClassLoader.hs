module ClassPath.ClassLoader
  ( ClassDictionary
  , ClassLoader(..)
  , ClassId(..)
  , makeBootstrapClassLoader
  , makeAppClassLoader
  , makeSystemClassLoader
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
import           Data.List
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

makeClassLoader :: ClassLoaderType -> ClassLoader
makeClassLoader loaderType = ClassLoader loaderType makeUniqueId HashMap.empty

makeBootstrapClassLoader :: ClassLoader
makeBootstrapClassLoader = makeClassLoader BootstrapClassLoader

makeSystemClassLoader :: ClassLoader
makeSystemClassLoader = makeClassLoader SystemClassLoader

makeAppClassLoader :: ClassLoader
makeAppClassLoader = makeClassLoader AppClassLoader

loadNewClass :: JavaClassName -> IO (Either String JavaClass)
loadNewClass javaName = do
  maybeFile <- searchClassPath javaName
  case maybeFile of
    Right file -> do
      clazz <- ClassFile.loadClassFromFile file
      return $ Right clazz
    Left err -> return $ Left err

loadClass :: ClassLoader -> JavaClassName -> IO (Either String (ClassLoader, JavaClass))
loadClass cl@(ClassLoader clType id classes) name =
  case lookupClass classes classId of
    Just clazz -> return $ Right (cl, clazz)
    Nothing -> do
      newClazz <- loadNewClass name
      return $ do
        clazz <- newClazz
        let newClassLoader = ClassLoader clType id (saveClass classes classId clazz)
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

detectLoader :: JavaClassName -> ClassLoaderType
detectLoader javaName
  | any (`isPrefixOf` name) ["javax.", "sun.", "java.ext."] = SystemClassLoader
  | "java." `isPrefixOf` name = BootstrapClassLoader
  | otherwise = AppClassLoader
  where
    name = unpackClassName javaName

loadClassJ :: JavaClassName -> JavaContext JavaClass
loadClassJ javaName = do
  vm <- getJavaVMM
  let loaderType = detectLoader javaName
  let cl = getClassLoaderJ loaderType vm
  result <- liftIO =<< loadClass cl <$> return javaName
  case result of
    Right (newCl, clazz) -> do
      setClassLoaderM loaderType newCl
      return clazz
    Left err -> throwError err
