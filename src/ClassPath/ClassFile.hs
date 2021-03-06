module ClassPath.ClassFile
  -- * Class declarations
  ( JavaClass(..)
  , classMethods
  , loadClassFromFile
  , lookupMethod
  , prettyClass
  , getClass
  -- * Field declarations
  , JavaField(..)
  , Visibility(..)
  , Attribute(..)
  -- * Method declarations
  , JavaMethod(..)
  , methodName
  , methodParameterTypes
  , methodParameterIndexes
  , localIndexOfParameter
  , methodReturnType
  , methodMaxLocals
  , methodIsNative
  , methodIsAbstract
  , MethodBody(..)
  , methodExceptionTable
  , MethodId(..)
  , makeMethodId
  -- ** Instruction declarations
  , LocalVariableIndex
  , LocalVariableTableEntry(..)
  , PC
  , lookupInstruction
  , nextPc
  -- ** Exception table declarations
  , ExceptionTableEntry
  , catchType
  , startPc
  , endPc
  , handlerPc
  -- ** Misc utility functions/values
  , javaByteArrayType
  , javaCharArrayType
  , getElemType
  , javaIntArrayType
  , javaStringType
  , unparseMethodDescriptor
  , mainMethodId
  -- * Debugging information
  , hasDebugInfo
  , sourceLineNumberInfo
  , sourceLineNumberOrPrev
  , lookupLineStartPC
  , lookupLineMethodStartPC
  , localVariableEntries
  , lookupLocalVariableByIdx
  , lookupLocalVariableByName
  , prettyInst
  , slashesToDots
  -- * Re-exports
  -- ** Types
  , JavaType(..)
  , equivToInt32
  , isPrimitiveType
  , stackWidth
  , isFloatType
  , isRefType
  -- ** Instructions
  , FieldId(..)
  , Instruction(..)
  -- ** Class names
  , JavaClassName
  , packClassName
  , unpackClassName
  , ConstantPoolValue(..)
  ) where

import           ClassPath.Base
import           ClassPath.ControlFlowGraph
import           ClassPath.Types
import           Control.Applicative
import           Control.Exception          (assert)
import           Control.Monad
import           Data.Array                 (listArray, (!))
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.IEEE754
import           Data.Bits
import qualified Data.ByteString.Lazy       as L
import           Data.Char
import           Data.Int
import           Data.List
import qualified Data.Map                   as Map
import           Data.Maybe
import           Prelude                    hiding (read)
import           System.IO

-- Version of replicate with arguments convoluted for parser.
replicateN :: (Integral b, Monad m) => m a -> b -> m [a]
replicateN fn i = replicateM (fromIntegral i) fn

-- Convert [Attribute] to Map String Attribute
toAttrList :: [Attribute] -> Map.Map String Attribute
toAttrList as = Map.fromList (map (\a -> (attributeName a, a)) as)

----------------------------------------------------------------------
-- Type
parseTypeDescriptor :: String -> (JavaType, String)
parseTypeDescriptor ('B':rest) = (JavaByteType, rest)
parseTypeDescriptor ('C':rest) = (JavaCharType, rest)
parseTypeDescriptor ('D':rest) = (JavaDoubleType, rest)
parseTypeDescriptor ('F':rest) = (JavaFloatType, rest)
parseTypeDescriptor ('I':rest) = (JavaIntType, rest)
parseTypeDescriptor ('J':rest) = (JavaLongType, rest)
parseTypeDescriptor ('L':rest) = split rest []
  where
    split (';':rest') result = (JavaClassType (packClassName (reverse result)), rest')
    split (ch:rest') result = split rest' (ch : result)
    split _ _ = error "internal: unable to parse type descriptor"
parseTypeDescriptor ('S':rest) = (JavaShortType, rest)
parseTypeDescriptor ('Z':rest) = (JavaBooleanType, rest)
parseTypeDescriptor ('[':rest) = (JavaArrayType tp, result)
  where
    (tp, result) = parseTypeDescriptor rest
parseTypeDescriptor st = error ("Unexpected type descriptor string " ++ st)

----------------------------------------------------------------------
-- Method descriptors
parseMethodDescriptor :: String -> (Maybe JavaType, [JavaType])
parseMethodDescriptor ('(':rest) = impl rest []
  where
    impl ")V" types = (Nothing, reverse types)
    impl (')':rest') types = (Just $ fst $ parseTypeDescriptor rest', reverse types)
    impl text types =
      let (tp, rest') = parseTypeDescriptor text
       in impl rest' (tp : types)
parseMethodDescriptor _ = error "internal: unable to parse method descriptor"

unparseMethodDescriptor :: MethodId -> String
unparseMethodDescriptor (MethodId _ paramTys retTy) =
  "(" ++ concatMap typeToDesc paramTys ++ ")" ++ maybe "V" typeToDesc retTy
  where
    typeToDesc (JavaArrayType ty) = "[" ++ typeToDesc ty
    typeToDesc JavaBooleanType    = "Z"
    typeToDesc JavaByteType       = "B"
    typeToDesc JavaCharType       = "C"
    typeToDesc (JavaClassType cn) = "L" ++ unpackClassName cn ++ ";"
    typeToDesc JavaDoubleType     = "D"
    typeToDesc JavaFloatType      = "F"
    typeToDesc JavaIntType        = "I"
    typeToDesc JavaLongType       = "J"
    typeToDesc JavaShortType      = "S"
    typeToDesc JavaVoidType       = "V"

-- | Returns method key with the given name and descriptor.
makeMethodId ::
     String -- ^ Method name
  -> String -- ^ Method descriptor
  -> MethodId
makeMethodId name descriptor = MethodId name parameters returnType
  where
    (returnType, parameters) = parseMethodDescriptor descriptor

mainMethodId :: MethodId
mainMethodId = makeMethodId "main" "([Ljava/lang/String;)V"

-- Parses array of bytes from Java string
getJavaString :: [Word8] -> String
getJavaString [] = []
getJavaString (x:rest)
  | (x .&. 0x80) == 0 = chr (fromIntegral x) : getJavaString rest
getJavaString (x:y:rest)
  | (x .&. 0xE0) == 0xC0 && ((y .&. 0xC0) == 0x80) = chr i : getJavaString rest
  where
    i = (fromIntegral x .&. 0x1F) `shift` 6 + (fromIntegral y .&. 0x3F)
getJavaString (x:y:z:rest)
  | (x .&. 0xF0) == 0xE0 && ((y .&. 0xC0) == 0x80) && ((z .&. 0xC0) == 0x80) = chr i : getJavaString rest
  where
    i = (fromIntegral x .&. 15) `shift` 12 + (fromIntegral y .&. 63) `shift` 6 + (fromIntegral z .&. 63)
getJavaString _ = error "internal: unable to parse byte array for Java string"

getConstantPoolInfo :: Get [ConstantPoolInfo]
getConstantPoolInfo = do
  tag <- getWord8
  case tag of
    1 -- CONSTANT_Utf8
     -> do
      bytes <- replicateN getWord8 =<< getWord16be
      return [Utf8 $ getJavaString bytes]
    3 ---- CONSTANT_Integer
     -> do
      val <- get
      return [ConstantInteger val]
    4 ---- CONSTANT_Float
     -> do
      v <- getFloat32be
      return [ConstantFloat v]
    5 ---- CONSTANT_Long
     -> do
      val <- get
      return [Phantom, ConstantLong val]
    6 ---- CONSTANT_Double
     -> do
      val <- getFloat64be
      return [Phantom, ConstantDouble val]
    7 ---- CONSTANT_Class
     -> do
      index <- getWord16be
      return [ConstantClass index]
    8 ---- CONSTANT_String
     -> do
      index <- getWord16be
      return [ConstantString index]
    9 ---- CONSTANT_Fieldref
     -> do
      classIndex <- getWord16be
      nameTypeIndex <- getWord16be
      return [FieldRef classIndex nameTypeIndex]
    10 ---- CONSTANT_Methodref
     -> do
      classIndex <- getWord16be
      nameTypeIndex <- getWord16be
      return [MethodRef classIndex nameTypeIndex]
    11 ---- CONSTANT_InterfaceMethodref
     -> do
      classIndex <- getWord16be
      nameTypeIndex <- getWord16be
      return [InterfaceMethodRef classIndex nameTypeIndex]
    12 ---- CONSTANT_NameAndType
     -> do
      classIndex <- getWord16be
      nameTypeIndex <- getWord16be
      return [NameAndType classIndex nameTypeIndex]
    15 ---- CONSTANT_MethodHandle_info
     -> do
      referenceKind <- getWord8
      referenceIndex <- getWord16be
      return [MethodHandle referenceKind referenceIndex]
    16 ---- CONSTANT_MethodType_info
     -> do
      descriptorIndex <- getWord16be
      return [MethodType descriptorIndex]
    18 ---- CONSTANT_InvokeDynamic_info
     -> do
      bootstrapMethodIndex <- getWord16be
      nameTypeIndex <- getWord16be
      return [InvokeDynamic bootstrapMethodIndex nameTypeIndex]
    _ -> do
      position <- bytesRead
      error ("Unexpected constant " ++ show tag ++ " at position " ++ show position)

getConstantPool :: Get ConstantPool
getConstantPool = do
  poolCount <- getWord16be
  list <- parseList (poolCount - 1) []
  return $ listArray (1, poolCount - 1) list
  where
    parseList 0 result = return $ reverse result
    parseList n result = do
      info <- getConstantPoolInfo
      parseList (n - fromIntegral (length info)) (info ++ result)

poolUtf8 :: ConstantPool -> ConstantPoolIndex -> String
poolUtf8 cp i =
  case cp ! i of
    Utf8 s -> s
    v -> error $ "Index " ++ show i ++ " has value " ++ show v ++ " when string expected."

poolValue :: ConstantPool -> ConstantPoolIndex -> ConstantPoolValue
poolValue cp i =
  case cp ! i of
    ConstantClass j -> ClassRef (packClassName (cp `poolUtf8` j))
    ConstantDouble v -> Double v
    ConstantFloat v -> Float v
    ConstantInteger v -> Integer v
    ConstantLong v -> Long v
    ConstantString j -> String (cp `poolUtf8` j)
    v -> error ("Index " ++ show i ++ " has unexpected value " ++ show v ++ " when a constant was expected.")

poolClassType :: ConstantPool -> ConstantPoolIndex -> JavaType
poolClassType cp i =
  case cp ! i of
    ConstantClass j ->
      let typeName = poolUtf8 cp j
       in if head typeName == '['
            then fst (parseTypeDescriptor typeName)
            else JavaClassType (packClassName typeName)
    _ -> error ("Index " ++ show i ++ " is not a class reference.")

poolNameAndType :: ConstantPool -> ConstantPoolIndex -> (String, String)
poolNameAndType cp i =
  case cp ! i of
    NameAndType nameIndex typeIndex -> (poolUtf8 cp nameIndex, poolUtf8 cp typeIndex)
    _ -> error ("Index " ++ show i ++ " is not a name and type reference.")

poolFieldRef :: ConstantPool -> ConstantPoolIndex -> FieldId
poolFieldRef cp i =
  case cp ! i of
    FieldRef classIndex ntIndex ->
      let (name, fldDescriptor) = poolNameAndType cp ntIndex
          (fldType, []) = parseTypeDescriptor fldDescriptor
          JavaClassType cName = poolClassType cp classIndex
       in FieldId cName name fldType
    _ -> error ("Index " ++ show i ++ " is not a field reference.")

poolInterfaceMethodRef :: ConstantPool -> ConstantPoolIndex -> (JavaType, MethodId)
poolInterfaceMethodRef cp i =
  case cp ! i of
    InterfaceMethodRef classIndex ntIndex ->
      let (name, fieldDescriptor) = poolNameAndType cp ntIndex
          interfaceType = poolClassType cp classIndex
       in (interfaceType, makeMethodId name fieldDescriptor)
    _ -> error ("Index " ++ show i ++ " is not an interface method reference.")

poolMethodRef :: ConstantPool -> ConstantPoolIndex -> (JavaType, MethodId)
poolMethodRef cp i =
  case cp ! i of
    MethodRef classIndex ntIndex ->
      let (name, fieldDescriptor) = poolNameAndType cp ntIndex
          classType = poolClassType cp classIndex
       in (classType, makeMethodId name fieldDescriptor)
    _ -> error ("Index " ++ show i ++ " is not a method reference.")

_uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
_uncurry3 fn (a, b, c) = fn a b c

-- (getInstruction cp addr) returns a parser for the instruction
-- at the address addr.
getInstruction :: ConstantPool -> PC -> Get Instruction
getInstruction cp address = do
  op <- getWord8
  case op of
    0x00 -> return Nop
    0x01 -> return AconstNull
    0x02 -> return $ Ldc $ Integer (-1)
    0x03 -> return $ Ldc $ Integer 0
    0x04 -> return $ Ldc $ Integer 1
    0x05 -> return $ Ldc $ Integer 2
    0x06 -> return $ Ldc $ Integer 3
    0x07 -> return $ Ldc $ Integer 4
    0x08 -> return $ Ldc $ Integer 5
    0x09 -> return $ Ldc $ Long 0
    0x0A -> return $ Ldc $ Long 1
    0x0B -> return $ Ldc $ Float 0.0
    0x0C -> return $ Ldc $ Float 1.0
    0x0D -> return $ Ldc $ Float 2.0
    0x0E -> return $ Ldc $ Double 0.0
    0x0F -> return $ Ldc $ Double 1.0
    0x10 -> Ldc . Integer . fromIntegral <$> (get :: Get Int8)
    0x11 -> Ldc . Integer . fromIntegral <$> (get :: Get Int16)
    0x12 -> Ldc . poolValue cp . fromIntegral <$> getWord8
    0x13 -> Ldc . poolValue cp <$> getWord16be
    0x14 -> Ldc . poolValue cp <$> getWord16be
    0x15 -> Iload . fromIntegral <$> getWord8
    0x16 -> Lload . fromIntegral <$> getWord8
    0x17 -> Fload . fromIntegral <$> getWord8
    0x18 -> Dload . fromIntegral <$> getWord8
    0x19 -> Aload . fromIntegral <$> getWord8
    0x1A -> return (Iload 0)
    0x1B -> return (Iload 1)
    0x1C -> return (Iload 2)
    0x1D -> return (Iload 3)
    0x1E -> return (Lload 0)
    0x1F -> return (Lload 1)
    0x20 -> return (Lload 2)
    0x21 -> return (Lload 3)
    0x22 -> return (Fload 0)
    0x23 -> return (Fload 1)
    0x24 -> return (Fload 2)
    0x25 -> return (Fload 3)
    0x26 -> return (Dload 0)
    0x27 -> return (Dload 1)
    0x28 -> return (Dload 2)
    0x29 -> return (Dload 3)
    0x2A -> return (Aload 0)
    0x2B -> return (Aload 1)
    0x2C -> return (Aload 2)
    0x2D -> return (Aload 3)
    0x2E -> return Iaload
    0x2F -> return Laload
    0x30 -> return Faload
    0x31 -> return Daload
    0x32 -> return Aaload
    0x33 -> return Baload
    0x34 -> return Caload
    0x35 -> return Saload
    0x36 -> Istore . fromIntegral <$> getWord8
    0x37 -> Lstore . fromIntegral <$> getWord8
    0x38 -> Fstore . fromIntegral <$> getWord8
    0x39 -> Dstore . fromIntegral <$> getWord8
    0x3A -> Astore . fromIntegral <$> getWord8
    0x3B -> return (Istore 0)
    0x3C -> return (Istore 1)
    0x3D -> return (Istore 2)
    0x3E -> return (Istore 3)
    0x3F -> return (Lstore 0)
    0x40 -> return (Lstore 1)
    0x41 -> return (Lstore 2)
    0x42 -> return (Lstore 3)
    0x43 -> return (Fstore 0)
    0x44 -> return (Fstore 1)
    0x45 -> return (Fstore 2)
    0x46 -> return (Fstore 3)
    0x47 -> return (Dstore 0)
    0x48 -> return (Dstore 1)
    0x49 -> return (Dstore 2)
    0x4A -> return (Dstore 3)
    0x4B -> return (Astore 0)
    0x4C -> return (Astore 1)
    0x4D -> return (Astore 2)
    0x4E -> return (Astore 3)
    0x4F -> return Iastore
    0x50 -> return Lastore
    0x51 -> return Fastore
    0x52 -> return Dastore
    0x53 -> return Aastore
    0x54 -> return Bastore
    0x55 -> return Castore
    0x56 -> return Sastore
    0x57 -> return Pop
    0x58 -> return Pop2
    0x59 -> return Dup
    0x5A -> return Dup_x1
    0x5B -> return Dup_x2
    0x5C -> return Dup2
    0x5D -> return Dup2_x1
    0x5E -> return Dup2_x2
    0x5F -> return Swap
    0x60 -> return Iadd
    0x61 -> return Ladd
    0x62 -> return Fadd
    0x63 -> return Dadd
    0x64 -> return Isub
    0x65 -> return Lsub
    0x66 -> return Fsub
    0x67 -> return Dsub
    0x68 -> return Imul
    0x69 -> return Lmul
    0x6A -> return Fmul
    0x6B -> return Dmul
    0x6C -> return Idiv
    0x6D -> return Ldiv
    0x6E -> return Fdiv
    0x6F -> return Ddiv
    0x70 -> return Irem
    0x71 -> return Lrem
    0x72 -> return Frem
    0x73 -> return Drem
    0x74 -> return Ineg
    0x75 -> return Lneg
    0x76 -> return Fneg
    0x77 -> return Dneg
    0x78 -> return Ishl
    0x79 -> return Lshl
    0x7A -> return Ishr
    0x7B -> return Lshr
    0x7C -> return Iushr
    0x7D -> return Lushr
    0x7E -> return Iand
    0x7F -> return Land
    0x80 -> return Ior
    0x81 -> return Lor
    0x82 -> return Ixor
    0x83 -> return Lxor
    0x84 -> do
      index <- getWord8
      constant <- get :: Get Int8
      return (Iinc (fromIntegral index) (fromIntegral constant))
    0x85 -> return I2l
    0x86 -> return I2f
    0x87 -> return I2d
    0x88 -> return L2i
    0x89 -> return L2f
    0x8A -> return L2d
    0x8B -> return F2i
    0x8C -> return F2l
    0x8D -> return F2d
    0x8E -> return D2i
    0x8F -> return D2l
    0x90 -> return D2f
    0x91 -> return I2b
    0x92 -> return I2c
    0x93 -> return I2s
    0x94 -> return Lcmp
    0x95 -> return Fcmpl
    0x96 -> return Fcmpg
    0x97 -> return Dcmpl
    0x98 -> return Dcmpg
    0x99 -> Ifeq . (address +) . fromIntegral <$> (get :: Get Int16)
    0x9A -> Ifne . (address +) . fromIntegral <$> (get :: Get Int16)
    0x9B -> Iflt . (address +) . fromIntegral <$> (get :: Get Int16)
    0x9C -> Ifge . (address +) . fromIntegral <$> (get :: Get Int16)
    0x9D -> Ifgt . (address +) . fromIntegral <$> (get :: Get Int16)
    0x9E -> Ifle . (address +) . fromIntegral <$> (get :: Get Int16)
    0x9F -> If_icmpeq . (address +) . fromIntegral <$> (get :: Get Int16)
    0xA0 -> If_icmpne . (address +) . fromIntegral <$> (get :: Get Int16)
    0xA1 -> If_icmplt . (address +) . fromIntegral <$> (get :: Get Int16)
    0xA2 -> If_icmpge . (address +) . fromIntegral <$> (get :: Get Int16)
    0xA3 -> If_icmpgt . (address +) . fromIntegral <$> (get :: Get Int16)
    0xA4 -> If_icmple . (address +) . fromIntegral <$> (get :: Get Int16)
    0xA5 -> If_acmpeq . (address +) . fromIntegral <$> (get :: Get Int16)
    0xA6 -> If_acmpne . (address +) . fromIntegral <$> (get :: Get Int16)
    0xA7 -> Goto . (address +) . fromIntegral <$> (get :: Get Int16)
    0xA8 -> Jsr . (address +) . fromIntegral <$> (get :: Get Int16)
    0xA9 -> Ret . fromIntegral <$> getWord8
    0xAA -> do
      read <- bytesRead
      skip $ fromIntegral $ (4 - read `mod` 4) `mod` 4
      defaultBranch <- (address +) . fromIntegral <$> (get :: Get Int32)
      low <- get :: Get Int32
      high <- get :: Get Int32
      offsets <- replicateN ((address +) . fromIntegral <$> (get :: Get Int32)) (high - low + 1)
      return $ Tableswitch defaultBranch low high offsets
    0xAB -> do
      read <- bytesRead
      skip (fromIntegral ((4 - read `mod` 4) `mod` 4))
      defaultBranch <- get :: Get Int32
      count <- get :: Get Int32
      pairs <-
        replicateM (fromIntegral count) $ do
          v <- get :: Get Int32
          o <- get :: Get Int32
          return (v, ((address +) . fromIntegral) o)
      return $ Lookupswitch (address + fromIntegral defaultBranch) pairs
    0xAC -> return Ireturn
    0xAD -> return Lreturn
    0xAE -> return Freturn
    0xAF -> return Dreturn
    0xB0 -> return Areturn
    0xB1 -> return Return
    0xB2 -> Getstatic . poolFieldRef cp <$> getWord16be
    0xB3 -> Putstatic . poolFieldRef cp <$> getWord16be
    0xB4 -> Getfield . poolFieldRef cp <$> getWord16be
    0xB5 -> Putfield . poolFieldRef cp <$> getWord16be
    0xB6 -> do
      index <- getWord16be
      let (classType, key) = poolMethodRef cp index
      return $ Invokevirtual classType key
    0xB7 -> do
      index <- getWord16be
      let (classType, key) = poolMethodRef cp index
      return $ Invokespecial classType key
    0xB8 -> do
      index <- getWord16be
      let (JavaClassType cName, key) = poolMethodRef cp index
       in return $ Invokestatic cName key
    0xB9 -> do
      index <- getWord16be
      [_, _] <- replicateM 2 getWord8
      let (JavaClassType cName, key) = poolInterfaceMethodRef cp index
       in return $ Invokeinterface cName key
    0xBA -> do
      index <- getWord16be
      [_, _] <- replicateM 2 getWord8
      return $ Invokedynamic index
    0xBB -> do
      index <- getWord16be
      case poolClassType cp index of
        JavaClassType name -> return (New name)
        _                  -> error "internal: unexpected pool class type"
    0xBC -> do
      typeCode <- getWord8
      (return . Newarray . JavaArrayType)
        (case typeCode of
           4  -> JavaBooleanType
           5  -> JavaCharType
           6  -> JavaFloatType
           7  -> JavaDoubleType
           8  -> JavaByteType
           9  -> JavaShortType
           10 -> JavaIntType
           11 -> JavaLongType
           _  -> error "internal: invalid type code encountered")
    0xBD -> Newarray . JavaArrayType . poolClassType cp <$> get
    0xBE -> return Arraylength
    0xBF -> return Athrow
    0xC0 -> Checkcast . poolClassType cp <$> get
    0xC1 -> Instanceof . poolClassType cp <$> get
    0xC2 -> return Monitorenter
    0xC3 -> return Monitorexit
    -- Wide instruction
    0xC4 -> do
      embeddedOp <- getWord8
      case embeddedOp of
        0x15 -> Iload <$> getWord16be
        0x16 -> Lload <$> getWord16be
        0x17 -> Fload <$> getWord16be
        0x18 -> Dload <$> getWord16be
        0x19 -> Aload <$> getWord16be
        0x36 -> Istore <$> getWord16be
        0x37 -> Lstore <$> getWord16be
        0x38 -> Fstore <$> getWord16be
        0x39 -> Dstore <$> getWord16be
        0x3A -> Astore <$> getWord16be
        0x84 -> liftM2 Iinc getWord16be (get :: Get Int16)
        0xA9 -> Ret <$> getWord16be
        _ -> do
          position <- bytesRead
          error ("Unexpected wide op " ++ show op ++ " at position " ++ show (position - 2))
    0xC5 -> do
      classIndex <- getWord16be
      Multianewarray (poolClassType cp classIndex) <$> getWord8
    0xC6 -> Ifnull . (address +) . fromIntegral <$> (get :: Get Int16)
    0xC7 -> Ifnonnull . (address +) . fromIntegral <$> (get :: Get Int16)
    0xC8 -> Goto . (address +) . fromIntegral <$> (get :: Get Int32)
    0xC9 -> Jsr . (address +) . fromIntegral <$> (get :: Get Int32)
    _ -> do
      position <- bytesRead
      error ("Unexpected op " ++ show op ++ " at position " ++ show (position - 1))

-- Returns getter that parses attributes from stream and buckets them based on name.
splitAttributes :: ConstantPool -> [String] -> Get ([[L.ByteString]], [Attribute])
splitAttributes cp names = do
  count <- getWord16be
  impl count (replicate (length names) []) []
        -- (appendAt list-of-lists index val) adds val to front of list at
        -- index i in list-of-lists
  where
    appendAt (l:rest) 0 a = (l ++ [a]) : rest
    appendAt (first:rest) n a = first : appendAt rest (n - 1) a
    appendAt [] _ _ = error "internal: appendAt expects non-empty list"
        -- Parse values
    impl 0 values rest = return (values, reverse rest)
    impl n values rest = do
      nameIndex <- getWord16be
      len <- getWord32be
      let name = poolUtf8 cp nameIndex
       in case elemIndex name names of
            Just i -> do
              bytes <- getLazyByteString (fromIntegral len)
              impl (n - 1) (appendAt values i bytes) rest
            Nothing -> do
              bytes <- getByteString (fromIntegral len)
              impl (n - 1) values (Attribute name bytes : rest)

getField :: ConstantPool -> Get JavaField
getField cp = do
  accessFlags <- getWord16be
  name <- poolUtf8 cp <$> getWord16be
  fldType <- fst . parseTypeDescriptor . poolUtf8 cp <$> getWord16be
  ([constantValue, synthetic, deprecated, signature], userAttrs) <-
    splitAttributes cp ["ConstantValue", "Synthetic", "Deprecated", "Signature"]
  return $
    JavaField
      name
      fldType
      (case accessFlags .&. 0x7 of
         0x0000 -> Default
         0x0001 -> Public -- | ACC_PUBLIC
         0x0002 -> Private -- | ACC_PRIVATE
         0x0004 -> Protected -- | ACC_PROTECTED
         flags  -> error $ "Unexpected flags " ++ show flags)
      ((accessFlags .&. 0x0008) /= 0) -- | ACC_STATIC
      ((accessFlags .&. 0x0010) /= 0) -- | ACC_FINAL
      ((accessFlags .&. 0x0040) /= 0) -- | ACC_ACC_VOLATILE
      ((accessFlags .&. 0x0080) /= 0) -- | ACC_TRANSIENT
      (case constantValue of
         [bytes] -> Just $ poolValue cp $ runGet getWord16be bytes
         []      -> Nothing
         _       -> error "internal: unexpected constant value form")
                   -- Check for synthetic bit in flags and buffer
      ((accessFlags .&. 4096) /= 0 || not (null synthetic)) -- | ACC_SYNTHETIC
                   -- Deprecated flag
      (not (null deprecated))
                   -- Check for enum bit in flags
      ((accessFlags .&. 0x4000) /= 0) -- | ACC_ENUM
                   -- Signature
      (case signature of
         [bytes] -> Just $ poolUtf8 cp $ runGet getWord16be bytes
         []      -> Nothing
         _       -> error "internal: unexpected signature form")
      (toAttrList userAttrs)

----------------------------------------------------------------------
-- Exception table
getExceptionTableEntry :: ConstantPool -> Get ExceptionTableEntry
getExceptionTableEntry cp = do
  startPc' <- getWord16be
  endPc' <- getWord16be
  handlerPc' <- getWord16be
  catchType' <- getWord16be
  return
    (ExceptionTableEntry
       startPc'
       endPc'
       handlerPc'
       (if catchType' == 0
          then Nothing
          else Just (poolClassType cp catchType')))

-- Run Get Monad until end of string is reached and return list of results.
getInstructions :: ConstantPool -> PC -> Get InstructionStream
getInstructions cp count = do
  read <- bytesRead
  impl 0 read []
  where
    impl pos prevRead result =
      if pos == fromIntegral count
        then return (listArray (0, count - 1) (reverse result))
        else do
          inst <- getInstruction cp pos
          newRead <- bytesRead
          let dist = fromIntegral (newRead - prevRead)
              padding = replicate (fromIntegral (dist - 1)) Nothing
           in impl (pos + dist) newRead (padding ++ (Just inst : result))

getLineNumberTableEntries :: Get [(PC, Word16)]
getLineNumberTableEntries = do
  tableLength <- getWord16be
  replicateM
    (fromIntegral tableLength)
    (do startPc' <- getWord16be
        lineNumber <- getWord16be
        return (startPc', lineNumber))

parseLineNumberTable :: [L.ByteString] -> LineNumberTable
parseLineNumberTable buffers =
  let l = concatMap (runGet getLineNumberTableEntries) buffers
   in LNT {pcLineMap = Map.fromList l, linePCMap = Map.fromListWith min [(ln, pc) | (pc, ln) <- l]}

getLocalVariableTableEntries :: ConstantPool -> Get [LocalVariableTableEntry]
getLocalVariableTableEntries cp = do
  tableLength <- getWord16be
  replicateM
    (fromIntegral tableLength)
    (do startPc' <- getWord16be
        len <- getWord16be
        nameIndex <- getWord16be
        descriptorIndex <- getWord16be
        LocalVariableTableEntry
          startPc'
          len
          (poolUtf8 cp nameIndex)
          (fst $ parseTypeDescriptor $ poolUtf8 cp descriptorIndex) <$>
          getWord16be)

parseLocalVariableTable :: ConstantPool -> [L.ByteString] -> [LocalVariableTableEntry]
parseLocalVariableTable cp = concatMap (runGet $ getLocalVariableTableEntries cp)

getCode :: ConstantPool -> Get MethodBody
getCode cp = do
  maxStack <- getWord16be
  maxLocals <- getWord16be
  codeLength <- getWord32be
  instructions <- getInstructions cp (fromIntegral codeLength)
  exceptionTable <- getWord16be >>= replicateN (getExceptionTableEntry cp)
  ([lineNumberTables, localVariableTables], userAttrs) <- splitAttributes cp ["LineNumberTable", "LocalVariableTable"]
  return $
    Code
      maxStack
      maxLocals
      (buildControlFlowGraph exceptionTable instructions)
      exceptionTable
      (parseLineNumberTable lineNumberTables)
      (parseLocalVariableTable cp localVariableTables)
      (toAttrList userAttrs)

----------------------------------------------------------------------
getExceptions :: ConstantPool -> Get [JavaType]
getExceptions cp = do
  exceptionCount <- getWord16be
  replicateN (poolClassType cp <$> getWord16be) exceptionCount

getMethod :: ConstantPool -> Get JavaMethod
getMethod cp = do
  accessFlags <- getWord16be
  name <- poolUtf8 cp <$> getWord16be
  (returnType, parameterTypes) <- parseMethodDescriptor . poolUtf8 cp <$> getWord16be
  ([codeVal, exceptionsVal, syntheticVal, deprecatedVal], userAttrs) <-
    splitAttributes cp ["Code", "Exceptions", "Synthetic", "Deprecated"]
  let isStatic' = (accessFlags .&. 0x0008) /= 0 -- | ACC_STATIC
      isFinal = (accessFlags .&. 0x0010) /= 0 -- | ACC_FINAL
      isSynchronized' = (accessFlags .&. 0x0020) /= 0 -- | ACC_SYNCHRONIZED
      isAbstract = (accessFlags .&. 0x0400) /= 0 -- | ACC_ABSTRACT
      isStrictFp' = (accessFlags .&. 0x0800) /= 0 -- | ACC_STRICT
   in return $
      JavaMethod
        (MethodId name parameterTypes returnType)
        (case accessFlags .&. 0x7 of
           0x0000 -> Default
           0x0001 -> Public -- | ACC_PUBLIC
           0x0002 -> Private -- | ACC_PRIVATE
           0x0004 -> Protected -- | ACC_PROTECTED
           flags  -> error $ "Unexpected flags " ++ show flags)
        isStatic'
        isFinal
        isSynchronized'
        isStrictFp'
        (not $ null syntheticVal)
        (not $ null deprecatedVal)
        (if (accessFlags .&. 256) /= 0
           then NativeMethod
           else if isAbstract
                  then AbstractMethod
                  else case codeVal of
                         [bytes] -> runGet (getCode cp) bytes
                         _       -> error "Could not find code attribute")
        (case exceptionsVal of
           [bytes] -> Just (runGet (getExceptions cp) bytes)
           []      -> Nothing
           _       -> error "internal: unexpected expectionsVal form")
        (toAttrList userAttrs)

methodIsNative :: JavaMethod -> Bool
methodIsNative m =
  case methodBody m of
    NativeMethod -> True
    _            -> False

-- | Returns true if method is abstract.
methodIsAbstract :: JavaMethod -> Bool
methodIsAbstract m =
  case methodBody m of
    AbstractMethod -> True
    _              -> False

-- | Returns the name of a method.
methodName :: JavaMethod -> String
methodName = methodIdName . methodId

-- | Returns parameter types for method.
methodParameterTypes :: JavaMethod -> [JavaType]
methodParameterTypes = methodIdParameterTypes . methodId

-- | Returns a list containing the local variable index that each
-- parameter is stored in when the method is invoked. Non-static
-- methods reserve index 0 for the @self@ parameter.
methodParameterIndexes :: JavaMethod -> [LocalVariableIndex]
methodParameterIndexes m = init $ scanl next start params
  where
    params = methodParameterTypes m
    start =
      if methodIsStatic m
        then 0
        else 1
    next n JavaDoubleType = n + 2
    next n JavaLongType   = n + 2
    next n _              = n + 1

-- | Returns the local variable index that the parameter is stored in when
-- the method is invoked.
localIndexOfParameter :: JavaMethod -> Int -> LocalVariableIndex
localIndexOfParameter m i = assert (0 <= i && i < length offsets) $ offsets !! i
  where
    offsets = methodParameterIndexes m

-- | Return type of the method, or 'Nothing' for a void return type.
methodReturnType :: JavaMethod -> Maybe JavaType
methodReturnType = methodIdReturnType . methodId

-- (lookupInstruction method pc) returns instruction at pc in method.
lookupInstruction :: JavaMethod -> PC -> Instruction
lookupInstruction method pc =
  case methodBody method of
    Code _ _ cfg _ _ _ _ -> fromMaybe (error "internal: failed to index inst stream") (cfgInstByPC cfg pc)
    _ -> error ("Method " ++ show method ++ " has no body")

-- Returns pc of next instruction.
nextPc :: JavaMethod -> PC -> PC
nextPc method pc =
  case methodBody method of
    Code _ _ cfg _ _ _ _ -> fromMaybe (error "internal: nextPc: no next instruction") (cfgNextPC cfg pc)
    _ -> error "internal: unexpected method body form"

--    trace ("nextPC: method = " ++ show method) $
--        nextPcPrim (toInstStream cfg) pc
-- | Returns maximum number of local variables in method.
methodMaxLocals :: JavaMethod -> LocalVariableIndex
methodMaxLocals method =
  case methodBody method of
    Code _ c _ _ _ _ _ -> c
    _                  -> error "internal: unexpected method body form"

-- | Returns true if method has debug informaiton available.
hasDebugInfo :: JavaMethod -> Bool
hasDebugInfo method =
  case methodBody method of
    Code _ _ _ _ lns lvars _ -> not (Map.null (pcLineMap lns) && null lvars)
    _                        -> False

methodLineNumberTable :: JavaMethod -> Maybe LineNumberTable
methodLineNumberTable me =
  case methodBody me of
    Code _ _ _ _ lns _ _ -> Just lns
    _                    -> Nothing

sourceLineNumberInfo :: JavaMethod -> [(Word16, PC)]
sourceLineNumberInfo me = maybe [] (Map.toList . pcLineMap) $ methodLineNumberTable me

-- | Returns source line number of an instruction in a method at a given PC,
-- or the line number of the nearest predecessor instruction, or 'Nothing' if
-- neither is available.
sourceLineNumberOrPrev :: JavaMethod -> PC -> Maybe Word16
sourceLineNumberOrPrev me pc =
  case methodBody me of
    Code _ _ _ _ lns _ _ ->
      case Map.splitLookup pc (pcLineMap lns) of
        (prs, Nothing, _)
          | not $ Map.null prs -> Just $ snd $ Map.findMax prs
          | otherwise -> Nothing
        (_, ln, _) -> ln
    _ -> error "internal: unexpected method body form"

-- | Returns the starting PC for the source at the given line number.
lookupLineStartPC :: JavaMethod -> Word16 -> Maybe PC
lookupLineStartPC me ln = do
  m <- methodLineNumberTable me
  Map.lookup ln (linePCMap m)

-- | Returns the enclosing method and starting PC for the source at the given line number.
lookupLineMethodStartPC :: JavaClass -> Word16 -> Maybe (JavaMethod, PC)
lookupLineMethodStartPC cl ln =
  case results of
    (p:_) -> return p
    []    -> mzero
  where
    results = do
      me <- Map.elems . classMethodMap $ cl
      case lookupLineStartPC me ln of
        Just pc -> return (me, pc)
        Nothing -> mzero

localVariableEntries :: JavaMethod -> PC -> [LocalVariableTableEntry]
localVariableEntries method pc =
  case methodBody method of
    Code _ _ _ _ _ lvars _ ->
      let matches e = localStart e <= pc && pc - localStart e <= localExtent e
       in filter matches lvars
    _ -> []

-- | Returns local variable entry at given PC and local variable index or
-- 'Nothing' if no mapping is found.
lookupLocalVariableByIdx :: JavaMethod -> PC -> LocalVariableIndex -> Maybe LocalVariableTableEntry
lookupLocalVariableByIdx method pc i = find (\e -> localIdx e == i) (localVariableEntries method pc)

-- | Returns local variable entry at given PC and local variable string or
-- 'Nothing' if no mapping is found.
lookupLocalVariableByName :: JavaMethod -> PC -> String -> Maybe LocalVariableTableEntry
lookupLocalVariableByName method pc name = find (\e -> localName e == name) (localVariableEntries method pc)

-- | Exception table entries for method.
methodExceptionTable :: JavaMethod -> [ExceptionTableEntry]
methodExceptionTable method =
  case methodBody method of
    Code _ _ _ table _ _ _ -> table
    _                      -> error "internal: unexpected method body form"

-- | Returns methods in class.
classMethods :: JavaClass -> [JavaMethod]
classMethods = Map.elems . classMethodMap

prettyClass :: JavaClass -> String
prettyClass cl =
  "Major Version: " ++
  show (majorVersion cl) ++
  "\n" ++
  "Minor Version: " ++
  show (minorVersion cl) ++
  "\n" ++
  "Constant Pool:\n" ++
  show (constantPool cl) ++
  "\n" ++
  (if classIsPublic cl
     then "public\n"
     else "") ++
  (if classIsFinal cl
     then "final\n"
     else "") ++
  (if classHasSuperAttribute cl
     then "super\n"
     else "") ++
  (if classIsInterface cl
     then "interface\n"
     else "") ++
  (if classIsAbstract cl
     then "abstract\n"
     else "") ++
  "This Class:    " ++
  show (className cl) ++
  "\n" ++
  "Super Class:   " ++
  show (superClass cl) ++
  "\n" ++
  "Interfaces:\n" ++
  showOnNewLines 2 (map show $ classInterfaces cl) ++
  "\n" ++
  "Fields:\n" ++
  showOnNewLines 2 (map show $ classFields cl) ++
  "\n" ++
  "Methods:\n" ++
  showOnNewLines 2 (map show $ classMethods cl) ++
  "\n" ++
  "Source file: " ++
  show (classSourceFile cl) ++ "\n" ++ "Attributes:\n" ++ showOnNewLines 2 (map show attributeList) ++ "\n"
  where
    attributeList = map snd (Map.toList (classAttributes cl))

-- | Binary parser for classes.
getClass :: Get JavaClass
getClass = do
  magic <- getWord32be
  when (magic /= 3405691582) $ error "Unexpected magic value"
  minorVersion' <- getWord16be
  majorVersion' <- getWord16be
  cp <- getConstantPool
  accessFlags <- getWord16be
  thisClass <- getReferenceName cp
  superClassIndex <- getWord16be
  interfaces <- getWord16be >>= replicateN (getReferenceName cp)
  fields <- getWord16be >>= replicateN (getField cp)
  methods <- getWord16be >>= replicateN (getMethod cp)
  ([sourceFile], userAttrs) <- splitAttributes cp ["SourceFile"]
  return $
    JavaClass
      majorVersion'
      minorVersion'
      cp
      ((accessFlags .&. 1) /= 0)
      ((accessFlags .&. 16) /= 0)
      ((accessFlags .&. 32) /= 0)
      ((accessFlags .&. 512) /= 0)
      ((accessFlags .&. 1024) /= 0)
      thisClass
      (if superClassIndex == 0
         then Nothing
         else case poolClassType cp superClassIndex of
                JavaClassType name -> Just name
                classType -> error ("Unexpected class type " ++ show classType))
      interfaces
      fields
      (Map.fromList (map (\m -> (methodId m, m)) methods))
      (case sourceFile of
         [bytes] -> Just $ poolUtf8 cp $ runGet getWord16be bytes
         []      -> Nothing
         _       -> error "internal: unexpected source file form")
      (toAttrList userAttrs)
  where
    getReferenceName cp = do
      index <- getWord16be
      case poolClassType cp index of
        JavaClassType name -> return name
        tp                 -> error ("Unexpected class type " ++ show tp)

-- | Returns method with given key in class or 'Nothing' if no method with that
-- key is found.
lookupMethod :: JavaClass -> MethodId -> Maybe JavaMethod
lookupMethod javaClass key = Map.lookup key (classMethodMap javaClass)

-- | Load and parse the class at the given path.
loadClassFromFile :: FilePath -> IO JavaClass
loadClassFromFile path = do
  handle <- openBinaryFile path ReadMode
  contents <- L.hGetContents handle
  let result = runGet getClass contents
   in result `seq` (hClose handle >> return result)

getElemType :: JavaType -> JavaType
getElemType (JavaArrayType t) = aux t
  where
    aux (JavaArrayType t') = aux t'
    aux t'                 = t'
getElemType _ = error "getArrElemType given non-array type"
