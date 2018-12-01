{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module ClassPath.Common
    -- * Class names
  ( JavaClassName
  , packClassName
  , unpackClassName
    -- * Types
  , JavaType(..)
  , javaStringType
  , javaIntArrayType
  , javaByteArrayType
  , javaCharArrayType
  , equivToInt32
  , isPrimitiveType
  , stackWidth
  , isFloatType
  , isRefType
  , prettyJavaType
    -- * FieldId
  , JavaFieldId(..)
  , prettyFieldId
    -- * MethodId
  , MethodId(..)
  , prettyMethodId
    -- * Instructions
  , ConstantPoolValue(..)
  , LocalVariableIndex
  , PC
  , Instruction(..)
  , prettyInst
  , canThrowException
  , isArrayLoad
  , isReturn
  , breaksControlFlow
  , InstructionStream
  , nextPcPrim
  , safeNextPcPrim
    -- * ExceptionTable
  , ExceptionTableEntry(..)
  , ExceptionTable
   -- * Miscellaneous
  , slashesToDots
  , dotsToSlashes
  ) where

import           Data.Array
import           Data.Int
import           Data.String      (IsString (..))
import           Data.Word
import           Prelude          hiding ((<>))
import           Text.PrettyPrint

-- | Replace / with .
slashesToDots :: String -> String
slashesToDots =
  map
    (\c ->
       if c == '/'
         then '.'
         else c)

-- | Replace . with /
dotsToSlashes :: String -> String
dotsToSlashes =
  map
    (\c ->
       if c == '.'
         then '/'
         else c)

-- | Name of a Java class, with names of packages separated by slashes.
newtype JavaClassName =
  JavaClassName String
  deriving (Eq, Ord, Show)

instance IsString JavaClassName where
  fromString s = JavaClassName s

-- | Make a class name from a string with packages separated by slashes.
packClassName :: String -> JavaClassName
packClassName s = JavaClassName s

-- | Print class name with names of packages separated by slashes.
unpackClassName :: JavaClassName -> String
unpackClassName (JavaClassName s) = s

-- | JVM Type.
data JavaType
  = JavaArrayType JavaType
  | JavaBooleanType
  | JavaByteType
  | JavaCharType
  | JavaClassType JavaClassName
  | JavaDoubleType
  | JavaFloatType
  | JavaIntType
  | JavaLongType
  | JavaShortType
  deriving (Eq, Ord)

javaStringType :: JavaType
javaStringType = JavaClassType "java/lang/String"

javaIntArrayType :: JavaType
javaIntArrayType = JavaArrayType JavaIntType

javaByteArrayType :: JavaType
javaByteArrayType = JavaArrayType JavaByteType

javaCharArrayType :: JavaType
javaCharArrayType = JavaArrayType JavaCharType

-- | Returns true if type can be represented as a 32-bit integer.
equivToInt32 :: JavaType -> Bool
equivToInt32 JavaBooleanType = True
equivToInt32 JavaByteType    = True
equivToInt32 JavaCharType    = True
equivToInt32 JavaIntType     = True
equivToInt32 JavaShortType   = True
equivToInt32 _           = False

-- | Returns true if Java type is a primitive type.  Primitive types are
-- the Boolean type or numeric types.
isPrimitiveType :: JavaType -> Bool
isPrimitiveType JavaBooleanType = True
isPrimitiveType JavaByteType    = True
isPrimitiveType JavaCharType    = True
isPrimitiveType JavaDoubleType  = True
isPrimitiveType JavaFloatType   = True
isPrimitiveType JavaIntType     = True
isPrimitiveType JavaLongType    = True
isPrimitiveType JavaShortType   = True
isPrimitiveType _           = False

-- | Returns number of bits that a Java type is expected to take on the stack.
-- | Type should be a primitive type.
-- | TODO: support slot size
stackWidth :: JavaType -> Int
stackWidth JavaBooleanType = 32
stackWidth JavaByteType    = 32
stackWidth JavaCharType    = 32
stackWidth JavaDoubleType  = 64
stackWidth JavaFloatType   = 32
stackWidth JavaIntType     = 32
stackWidth JavaLongType    = 64
stackWidth JavaShortType   = 32
stackWidth _           = error "internal: illegal type"

-- | Returns true if Java type denotes a floating point.
isFloatType :: JavaType -> Bool
isFloatType JavaFloatType  = True
isFloatType JavaDoubleType = True
isFloatType _          = False

-- | Returns true if Java type denotes a reference (i.e. array or class).
isRefType :: JavaType -> Bool
isRefType (JavaArrayType _) = True
isRefType (JavaClassType _) = True
isRefType _             = False

-- | Java Field
data JavaFieldId = JavaFieldId
  { fieldIdClass :: !JavaClassName -- Class name
  , fieldIdName  :: !String -- Field name
  , fieldIdType  :: !JavaType -- Field type
  } deriving (Eq, Ord, Show)

-- TODO: Refactor
prettyFieldId :: JavaFieldId -> String
prettyFieldId fldId = dotsToSlashes (unpackClassName (fieldIdClass fldId)) ++ "." ++ fieldIdName fldId

-- | A unique identifier for looking up a method in a class.
data MethodId = MethodId
  { methodIdName           :: String
  , methodIdParameterTypes :: [JavaType]
  , methodIdReturnType     :: Maybe JavaType
  } deriving (Eq, Ord, Show)

prettyMethodId :: MethodId -> Doc
prettyMethodId (MethodId name params ret) = text name <> (parens . commas . map prettyJavaType) params <> maybe "void" prettyJavaType ret
  where
    commas = sep . punctuate comma

-- | A value stored in the constant pool.
data ConstantPoolValue
  = Long Int64
  | Float Float
  | Double Double
  | Integer Int32
  | String String
  | ClassRef JavaClassName
  deriving (Eq, Show)

-- | A local variable index.
type LocalVariableIndex = Word16

-- | A program counter value.
type PC = Word16

-- | The JVM instruction set, according to the JVM Specification 8.
-- Some instructions have been generalized or merged with Haskell Styles:
--
-- Wide instructions goto_w, jsr_w and wide are removed since they are deprecated
-- Local variable instructions support wide indexes
-- anewarray replaced by generalized 'Newarray'
-- bipush and sipush are replaced with generalized 'Ldc'
-- dconst_*, fconst_*, iconst_*, lconst_* replaced with 'Ldc'
-- istore_*, lstore_*, fstore_*, dstore_*, astore_* merged with 'Istore', 'Lstore', etc.
-- ldc, ldc_w, and ldc2_w are merged into generalized 'Ldc'
data Instruction
  = Aaload -- | Load reference from array
  | Aastore -- | Store into reference array
  | Aconst_null -- | Push null
  | Aload LocalVariableIndex -- | Load reference from local variable
  | Areturn -- | Return reference from method
  | Arraylength -- | Get length of array
  | Astore LocalVariableIndex -- | Store reference into local variable
  | Athrow -- | Throw exception or error
  | Baload -- | Load byte or boolean from array
  | Bastore -- | Store into byte or boolean array
  | Caload -- | Load char from array
  | Castore -- | Store into char array
  | Checkcast JavaType -- | Check whether object is of given type
  | D2f -- | Convert double to float
  | D2i -- | Convert double to int
  | D2l -- | Convert double to long
  | Dadd -- | Add double
  | Daload -- | Load double from array
  | Dastore -- | Store into double array
  | Dcmpg -- | Compare double (> if NaN)
  | Dcmpl -- | Compare double (< if NaN)
  | Ddiv -- | Divide double
  | Dload LocalVariableIndex -- | Load double from local variable
  | Dmul -- | Multiply double
  | Dneg -- | Negate double
  | Drem -- | Remainder double
  | Dreturn -- | Return double from method
  | Dstore LocalVariableIndex -- | Store double into local variable
  | Dsub -- | Subtract double
  | Dup -- | Duplicate the top operand stack value
  | Dup_x1 -- | Duplicate the top operand stack value and insert two values down
  | Dup_x2 -- | Duplicate the top operand stack value and insert 2 or 3 values down
  | Dup2 -- | Duplicate the top one or two operand stack values
  | Dup2_x1 -- | Duplicate the top 1 or 2 operand stack values and insert 2 or 3 values down
  | Dup2_x2 -- | Duplicate the top 1 or 2 operand stack values and insert 2, 3, or 4 values down
  | F2d -- | Convert float to double
  | F2i -- | Convert float to int
  | F2l -- | Convert float to long
  | Fadd -- | Add float
  | Faload -- | Load float from array
  | Fastore -- | Store into float array
  | Fcmpg -- | Compare float (> if NaN)
  | Fcmpl -- | Compare float (< if NaN)
  | Fdiv -- | Divide float
  | Fload LocalVariableIndex -- | Load float from local variable
  | Fmul -- | Multiply float
  | Fneg -- | Negate float
  | Frem -- | Remainder float
  | Freturn -- | Return float from method
  | Fstore LocalVariableIndex -- | Store float into local variable
  | Fsub -- | Subtract float
  | Getfield JavaFieldId -- | Fetch field from object
  | Getstatic JavaFieldId -- | Get static field from class
  | Goto PC -- | Branch always
  | I2b -- | Convert int to byte
  | I2c -- | Convert int to char
  | I2d -- | Convert int to double
  | I2f -- | Convert int to float
  | I2l -- | Convert int to long
  | I2s -- | Convert int to short
  | Iadd -- | Add int
  | Iaload -- | Load int from array
  | Iand -- | Boolean AND int
  | Iastore -- | Store into int array
  | Idiv -- | Divide int
  | If_acmpeq PC -- | Branch if reference comparison is equal
  | If_acmpne PC -- | Branch if reference comparison is not equal
  | If_icmpeq PC -- | Branch if int comparison is equal
  | If_icmpne PC -- | Branch if int comparison is not equal
  | If_icmplt PC -- | Branch if int comparison is less than
  | If_icmpge PC -- | Branch if int comparison is greater than or equal
  | If_icmpgt PC -- | Branch if int comparison is greater than
  | If_icmple PC -- | Branch if int comparison is less than or equal
  | Ifeq PC -- | Branch if int equal to zero
  | Ifne PC -- | Branch if int not equal to zero
  | Iflt PC -- | Branch if int less than zero
  | Ifge PC -- | Branch if int greater than or equal to zero
  | Ifgt PC -- | Branch if int greater than zero
  | Ifle PC -- | Branch if int less than or equal to zero
  | Ifnonnull PC -- | Branch if reference not null
  | Ifnull PC -- | Branch if reference is null
  | Iinc LocalVariableIndex
         Int16 -- | Increment local variable by constant
  | Iload LocalVariableIndex -- | Load int from local variable
  | Imul -- | Multiply int
  | Ineg -- | Negate int
  | Instanceof JavaType -- | Determine if object is of given type
  | Invokedynamic Word16
  -- | Since we don't yet attempt to resolve invokedynamic targets,
  -- just store the constant pool index for the call site specifier
  | Invokeinterface JavaClassName
                    MethodId -- | Invoke interface method
  | Invokespecial JavaType
                  MethodId -- | Invoke instance method; special handling for
                                        -- superclass, private, and instance initialization
                                        -- method invocations
  | Invokestatic JavaClassName
                 MethodId -- | Invoke a class (static) method
  | Invokevirtual JavaType
                  MethodId -- | Invoke instance method; dispatch based on class
  | Ior -- | Boolean OR int
  | Irem -- | Remainder int
  | Ireturn -- | Return int from method
  | Ishl -- | Shift left int
  | Ishr -- | Arithmetic shift right int
  | Istore LocalVariableIndex -- | Store int into local variable
  | Isub -- | Subtract int
  | Iushr -- | Logical shift right int
  | Ixor -- | Boolean XOR int
  | Jsr PC -- | Jump subroutine
  | L2d -- | Convert long to double
  | L2f -- | Convert long to float
  | L2i -- | Convert long to int
  | Ladd -- | Add long
  | Laload -- | Load long from array
  | Land -- | Boolean AND long
  | Lastore -- | Store into long array
  | Lcmp -- | Compare long
  | Ldc ConstantPoolValue -- | Push item from runtime constant pool
  | Ldiv -- | Divide long
  | Lload LocalVariableIndex -- | Load long from local variable
  | Lmul -- | Multiply long
  | Lneg -- | Negate long
  | Lookupswitch PC
                 [(Int32, PC)] -- | Access jump table by key match and jump
                               {- default -}
                               {- (key, target) -}
  | Lor -- | Boolean OR long
  | Lrem -- | Remainder long
  | Lreturn -- | Return long from method
  | Lshl -- | Shift left long
  | Lshr -- | Arithmetic shift right long
  | Lstore LocalVariableIndex -- | Store long into local variable
  | Lsub -- | Subtract long
  | Lushr -- | Logical shift right long
  | Lxor -- | Boolean XOR long
  | Monitorenter -- | Enter monitor for object
  | Monitorexit -- | Exit monitor for object
  | Multianewarray JavaType
                   Word8 -- | Create new multidimensional array (element type, dimensions)
  | New JavaClassName -- | Create new object
  | Newarray JavaType -- | Create new array (The type is the type of the array.)
  | Nop -- | Do nothing
  | Pop -- | Pop the top operand stack value
  | Pop2 -- | Pop the top one or two operand stack values
  | Putfield JavaFieldId -- | Set field in object
  | Putstatic JavaFieldId -- | Set static field in class
  | Ret LocalVariableIndex -- | Return from subroutine
  | Return -- | Return void from method
  | Saload -- | Load short from array
  | Sastore -- | Store into short array
  | Swap -- | Swap the top two operand stack values
  | Tableswitch PC
                Int32
                Int32
                [PC] -- | Access jump table by index and jump
  deriving (Eq, Show)

-- TODO: improve this
prettyInst :: Instruction -> Doc
prettyInst = text . show

-- | An entry in the exception table for a method
data ExceptionTableEntry = ExceptionTableEntry
  { startPc   :: PC
  , endPc     :: PC
  , handlerPc :: PC
  , catchType :: Maybe JavaType
  } deriving (Eq, Show)

type ExceptionTable = [ExceptionTableEntry]

type InstructionStream = Array PC (Maybe Instruction)

--------------------------------------------------------------------------------
-- Utility functions
canThrowException :: Instruction -> Bool
canThrowException Arraylength {}     = True
canThrowException Checkcast {}       = True
canThrowException Getfield {}        = True
canThrowException Getstatic {}       = True
canThrowException Idiv {}            = True
canThrowException Invokeinterface {} = True
canThrowException Invokespecial {}   = True
canThrowException Invokestatic {}    = True
canThrowException Invokevirtual {}   = True
canThrowException Irem {}            = True
canThrowException Ldiv {}            = True
canThrowException Lrem {}            = True
canThrowException Monitorenter {}    = True
canThrowException Monitorexit {}     = True
canThrowException Multianewarray {}  = True
canThrowException Newarray {}        = True
canThrowException New {}             = True
canThrowException Putfield {}        = True
canThrowException Putstatic {}       = True
canThrowException Athrow {}          = True
canThrowException inst               = isArrayLoad inst || isReturn inst

isArrayLoad :: Instruction -> Bool
isArrayLoad Aaload {}  = True
isArrayLoad Aastore {} = True
isArrayLoad Baload {}  = True
isArrayLoad Bastore {} = True
isArrayLoad Caload {}  = True
isArrayLoad Castore {} = True
isArrayLoad Daload {}  = True
isArrayLoad Dastore {} = True
isArrayLoad Faload {}  = True
isArrayLoad Fastore {} = True
isArrayLoad Iaload {}  = True
isArrayLoad Iastore {} = True
isArrayLoad Laload {}  = True
isArrayLoad Lastore {} = True
isArrayLoad Saload {}  = True
isArrayLoad Sastore {} = True
isArrayLoad _          = False

isReturn :: Instruction -> Bool
isReturn Areturn {} = True
isReturn Dreturn {} = True
isReturn Freturn {} = True
isReturn Ireturn {} = True
isReturn Lreturn {} = True
isReturn Return {}  = True
isReturn _          = False

breaksControlFlow :: Instruction -> Bool
breaksControlFlow Jsr {}    = True
breaksControlFlow Ret {}    = True
breaksControlFlow Goto {}   = True
breaksControlFlow Athrow {} = True
breaksControlFlow inst      = isReturn inst

nextPcPrim :: InstructionStream -> PC -> PC
nextPcPrim stream pc = findNext stream (pc + 1)
  where
    findNext is i =
      case (is ! i) of
        Just _  -> i
        Nothing -> findNext is (i + 1)

safeNextPcPrim :: InstructionStream -> PC -> Maybe PC
safeNextPcPrim stream pc
  | pc <= snd (bounds stream) = Just $ nextPcPrim stream pc
  | otherwise = Nothing

--------------------------------------------------------------------------------
-- Instances
instance Show JavaType where
  show JavaByteType       = "byte"
  show JavaCharType       = "char"
  show JavaDoubleType     = "double"
  show JavaFloatType      = "float"
  show JavaIntType        = "int"
  show JavaLongType       = "long"
  show (JavaClassType cn) = slashesToDots (unpackClassName cn)
  show JavaShortType      = "short"
  show JavaBooleanType    = "boolean"
  show (JavaArrayType tp) = (show tp) ++ "[]"

prettyJavaType :: JavaType -> Doc
prettyJavaType = text . show
