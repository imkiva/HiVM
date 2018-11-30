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
  minorVersion <- getWord16
  majorVersion <- getWord16
  poolCount <- getWord16
  pool <- parseConstantPool
  accessFlags <- getWord16
  thisClass <- getWord16
  superClass <- getWord16
  interfaceCount <- getWord16
  interfaces <- parseInterfaces
  fieldCount <- getWord16
  fields <- parseFields
  methodCount <- getWord16
  methods <- parseMethods
  attrCount <- getWord16
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
    attrCount <$>
    parseAttrs

parseClassMagic :: Get ()
parseClassMagic = do
  magic <- getWord32
  when (magic /= 0xCAFEBABE) $ fail "Invalid magic number for Java class, exptected 0xCAFEBABE"

parseConstantPool :: Get [ClassConstantPoolEntry]
parseConstantPool = undefined

parseInterfaces :: Get [Word16]
parseInterfaces = undefined

parseFields :: Get [ClassFileFieldInfo]
parseFields = undefined

parseMethods :: Get [ClassFileMethodInfo]
parseMethods = undefined

parseAttrs :: Get [AttributeInfo]
parseAttrs = undefined

parseClassFile :: L.ByteString -> ClassFile
parseClassFile = runGet parseJavaClass
