{-# LANGUAGE GADTs #-}

module ClassFile.ClassFile where

import           Data.Word

data ClassConstantPoolEntry = ClassConstantPoolEntry
  {
  }

data ClassFileFieldInfo = ClassFileFieldInfo
  { fieldInfoAccessFlag      :: Word16
  , fieldInfoNameIndex       :: Word16
  , fieldInfoDescriptorIndex :: Word16
  , fieldInfoAttributeCount  :: Word16
  , fieldInfoAttributes      :: [ClassFileAttributeInfo]
  }

data ClassFileMethodInfo = ClassFileMethodInfo
  { methodInfoAccessFlag      :: Word16
  , methodInfoNameIndex       :: Word16
  , methodInfoDescriptorIndex :: Word16
  , methodInfoAttributeCount  :: Word16
  , methodInfoAttributes      :: [ClassFileAttributeInfo]
  }

data ClassFileAttributeInfo = ClassFileAttributeInfo
  {
  }

data ClassFile = ClassFile
  { classFileMagic               :: Word32
  , classFileMajorVersion        :: Word16
  , classFileMinorVersion        :: Word16
  , classFileConstantPoolCount   :: Word16
  , classFileConstantPoolEntries :: [ClassConstantPoolEntry]
  , classFileAccessFlags         :: Word16
  , classFileThisClassIndex      :: Word16
  , classFileSuperClassIndex     :: Word16
  , classFileInterfaceCount      :: Word16
  , classFileInterfaces          :: [Word16]
  , classFileFieldCount          :: Word16
  , classFileFields              :: [ClassFileFieldInfo]
  , classFileMethodCount         :: Word16
  , classFileMethods             :: [ClassFileMethodInfo]
  , classFileAttributesCount     :: Word16
  , classFileAttributes          :: [ClassFileAttributeInfo]
  }
