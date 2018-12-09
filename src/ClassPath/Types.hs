{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module ClassPath.Types where

import           ClassPath.Base
import           ClassPath.ControlFlowGraph
import           Data.Array
import qualified Data.ByteString            as B
import           Data.Hashable
import qualified Data.HashMap.Strict        as HashMap
import           Data.Int
import           Data.Map                   (Map)
import           Data.Word
import           Prelude                    hiding ((<>))
import qualified Utils.UniqueId             as UniqueId

----------------------------------------------------------------------
-- Visibility
-- | Visibility of a field.
data Visibility
  = Default
  | Private
  | Protected
  | Public
  deriving (Eq)

instance Show Visibility where
  show Default   = "default"
  show Private   = "private"
  show Protected = "protected"
  show Public    = "public"

----------------------------------------------------------------------
-- Class declarations
-- | A Java class or interface.
data JavaClass = JavaClass
  { majorVersion           :: Word16
  , minorVersion           :: Word16
  , constantPool           :: ConstantPool
  , classIsPublic          :: Bool
  , classIsFinal           :: Bool
  , classHasSuperAttribute :: Bool
  , classIsInterface       :: Bool
  , classIsAbstract        :: Bool
  , className              :: JavaClassName
  , superClass             :: Maybe JavaClassName
  , classInterfaces        :: [JavaClassName]
  , classFields            :: [JavaField]
  , classMethodMap         :: Map MethodId JavaMethod
  , classSourceFile        :: Maybe String
  , classAttributes        :: [Attribute]
  } deriving (Show)

----------------------------------------------------------------------
-- Field declarations
-- | A field of a class.
data JavaField = JavaField
  { fieldName          :: String
  , fieldType          :: JavaType
  , fieldVisibility    :: Visibility
  , fieldIsStatic      :: Bool
  , fieldIsFinal       :: Bool
  , fieldIsVolatile    :: Bool
  , fieldIsTransient   :: Bool
  , fieldConstantValue :: Maybe ConstantPoolValue
  , fieldIsSynthetic   :: Bool
  , fieldIsDeprecated  :: Bool
  , fieldIsEnum        :: Bool
  , fieldSignature     :: Maybe String
  , fieldAttributes    :: [Attribute]
  } deriving (Show)

----------------------------------------------------------------------
-- Attributes
-- | An uninterpreted user-defined attribute in the class file.
data Attribute = Attribute
  { attributeName :: String
  , attributeData :: B.ByteString
  } deriving (Eq, Show)

-- Method definitions
data JavaMethod = JavaMethod
  { methodId             :: MethodId
  , methodVisibility     :: Visibility
  , methodIsStatic       :: Bool
  , methodIsFinal        :: Bool
  , methodIsSynchronized :: Bool
  , methodIsStrictFp     :: Bool
  , methodIsSynthetic    :: Bool
  , methodIsDeprecated   :: Bool
  , methodBody           :: MethodBody
  , methodExceptions     :: Maybe [JavaType]
  , methodAttributes     :: [Attribute]
  } deriving (Eq, Show)

instance Ord JavaMethod where
  compare m1 m2 = compare (methodId m1) (methodId m2)

----------------------------------------------------------------------
-- LocalVariableTableEntry
data LocalVariableTableEntry = LocalVariableTableEntry
  { localStart  :: PC -- Start PC
  , localExtent :: PC -- length
  , localName   :: String -- Name
  , localType   :: JavaType -- Type of local variable
  , localIdx    :: LocalVariableIndex -- Index of local variable
  } deriving (Eq, Show)

-- Maps pc and local variable index to name and type of variable in source.
type LocalVariableTable = [LocalVariableTableEntry]

----------------------------------------------------------------------
-- LineNumberTable
data LineNumberTable = LNT
  { pcLineMap :: Map PC Word16
  , linePCMap :: Map Word16 PC
  } deriving (Eq, Show)

----------------------------------------------------------------------
-- Method body
data MethodBody
  = Code { codeMaxStack       :: Word16
         , codeMaxLocals      :: Word16
         , codeControlFlow    :: ControlFlowGraph
         , codeExceptions     :: [ExceptionTableEntry]
         , codeLineNumbers    :: LineNumberTable
         , codeLocalVariables :: LocalVariableTable
         , codeAttributes     :: [Attribute] }
  | AbstractMethod
  | NativeMethod
  deriving (Eq, Show)

----------------------------------------------------------------------
-- ConstantPool
data ConstantPoolInfo
  = ConstantClass Word16
  | FieldRef Word16
             Word16
  | MethodRef Word16
              Word16
  | InterfaceMethodRef Word16
                       Word16
  | ConstantString Word16
  | ConstantInteger Int32
  | ConstantFloat Float
  | ConstantLong Int64
  | ConstantDouble Double
  | NameAndType Word16
                Word16
  | Utf8 String
  | MethodHandle Word8
                 Word16
  | MethodType Word16
  | InvokeDynamic Word16
                  Word16
    -- | Used for gaps after Long and double entries
  | Phantom
  deriving (Show)

type ConstantPoolIndex = Word16

type ConstantPool = Array ConstantPoolIndex ConstantPoolInfo

----------------------------------------------------------------------
-- ClassLoader
type ClassDictionary = HashMap.HashMap ClassId JavaClass

newtype ClassId =
  ClassId JavaClassName
  deriving (Eq, Ord, Show)

data ClassLoaderType
  = BootstrapClassLoader
  | SystemClassLoader
  | AppClassLoader
  deriving (Eq, Ord, Show)

data ClassLoader = ClassLoader
  { getLoaderType    :: ClassLoaderType
  , getLoaderId      :: UniqueId.UniqueId
  , getLoadedClasses :: ClassDictionary
  } deriving (Show)

instance Eq ClassLoader where
  (ClassLoader typeL idL _) == (ClassLoader typeR idR _) = typeL == typeR && idL == idR

instance Hashable ClassId where
  hashWithSalt salt (ClassId name) = hashWithSalt salt (unpackClassName name)
