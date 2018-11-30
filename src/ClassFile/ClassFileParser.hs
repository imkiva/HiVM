module ClassFile.ClassFileParser where

import           ClassFile.ClassFile
import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Word

getWord16 :: Get Word16
getWord16 = getWord16be

getWord32 :: Get Word32
getWord32 = getWord32be

parseJavaClass :: Get ClassFile
parseJavaClass = do
  parseClassMagic
  [minorVersion, majorVersion] <- replicateM 2 getWord16
  (poolCount, pool) <- parseConstantPool
  [accessFlags, thisClass, superClass] <- replicateM 3 getWord16
  (interfaceCount, interfaces) <- parseInterfaces
  (fieldCount, fields) <- parseFields
  (methodCount, methods) <- parseMethods
  (attrCount, attrs) <- parseAttrs
  return $
    ClassFile
      majorVersion
      minorVersion
      poolCount
      pool
      accessFlags
      thisClass
      superClass
      interfaceCount
      interfaces
      fieldCount
      fields
      methodCount
      methods
      attrCount
      attrs

parseClassMagic :: Get ()
parseClassMagic = do
  magic <- getWord32
  when (magic /= 0xCAFEBABE) $ fail "Invalid magic number for Java class, exptected 0xCAFEBABE"

parseConstantPool :: Get (Word16, [ClassConstantPoolEntry])
parseConstantPool = undefined

parseInterfaces :: Get (Word16, [Word16])
parseInterfaces = do
  count <- getWord16
  return (count, undefined)

parseFields :: Get (Word16, [ClassFileFieldInfo])
parseFields = do
  count <- getWord16
  return (count, undefined)

parseMethods :: Get (Word16, [ClassFileMethodInfo])
parseMethods = do
  count <- getWord16
  return (count, undefined)

parseAttrs :: Get (Word16, [AttributeInfo])
parseAttrs = do
  count <- getWord16
  return (count, undefined)

parseClassFile :: L.ByteString -> ClassFile
parseClassFile = runGet parseJavaClass
