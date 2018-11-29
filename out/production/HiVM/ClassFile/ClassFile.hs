{-# LANGUAGE GADTs #-}
module ClassFile.ClassFile where

import Data.Word

data ClassConstantPoolEntry = ClassConstantPoolEntry {}

data ClassFileFieldInfo = ClassFileFieldInfo {}

data ClassFile = ClassFile {
  classFileMagic :: Word32,
  classFileMajorVersion :: Word16,
  classFileMinorVersion :: Word16,
  classFileConstantPoolCount :: Word16,
  classFileConstantPoolEntries :: [ClassConstantPoolEntry],
  classFileAccessFlags :: Word16,
  classFileThisClassIndex :: Word16,
  classFileSuperClassIndex :: Word16,
  classFileInterfaceCount :: Word16,
  classFileInterfaces :: [Word16],
  classFileFieldCount :: Word16,
  classFileFields :: [ClassFileFieldInfo],
}
