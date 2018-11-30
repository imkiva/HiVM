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
  , fieldInfoAttributes      :: [AttributeInfo]
  }

data ClassFileMethodInfo = ClassFileMethodInfo
  { methodInfoAccessFlag      :: Word16
  , methodInfoNameIndex       :: Word16
  , methodInfoDescriptorIndex :: Word16
  , methodInfoAttributeCount  :: Word16
  , methodInfoAttributes      :: [AttributeInfo]
  }

data AttributeInfoHeader = AttributeInfoHeader
  { attributeNameIndex :: Word16
  , attributeLength    :: Word32
  }

data AttributeInfo
  = AttributeConstantValue { attributeHeader        :: AttributeInfoHeader
                           , attributeConstantIndex :: Word16 }
  | AttributeCode { attributeHeader          :: AttributeInfoHeader
                  , codeMaxStack             :: Word16
                  , codeMaxLocals            :: Word16
                  , codeLength               :: Word16
                  , codeByteCodes            :: [Word8]
                  , codeExceptionTableLength :: Word16
                  , codeExceptionTable       :: [ExceptionTable]
                  , codeAttributeCount       :: Word16
                  , codeAttributes           :: [AttributeInfo] }
  | AttributeStackMapTable { attributeHeader :: AttributeInfoHeader
                           , stackMapCount   :: Word16
                           , stackMapEntries :: [SMTFrame] }
  | AttributeException { attributeHeader       :: AttributeInfoHeader
                       , exceptionCount        :: Word16
                       , exceptionClassIndexes :: [Word16] }
  | AttributeInnerClasses { attributeHeader  :: AttributeInfoHeader
                          , classesInfoCount :: Word16
                          , classesInfo      :: [ClassesInfo] }
  | AttributeEnclosingMethod { attributeHeader :: AttributeInfoHeader
                             , emClassIndex    :: Word16
                             , emMethodIndex   :: Word16 }
  | AttributeSynthetic { attributeHeader :: AttributeInfoHeader }
  | AttributeSignature { attributeHeader :: AttributeInfoHeader
                       , signatureIndex  :: Word16 }
  | AttributeSourceFile { attributeHeader :: AttributeInfoHeader
                        , sourceFileIndex :: Word16 }
  | AttributeSourceDebugAttribute { attributeHeader :: AttributeInfoHeader
                                  , sourceDebugExts :: [Word8] }
  | AttributeLineNumberTable { attributeHeader :: AttributeInfoHeader
                             , lntLength       :: Word16
                             , lineNumbers     :: [LineNumberTable] }
  | AttributeLocalVariableTable { attributeHeader :: AttributeInfoHeader
                                , lvtLength       :: Word16
                                , localVariables  :: [LocalVariableTable] }
  | AttributeLocalVariableTypeTable { attributeHeader :: AttributeInfoHeader
                                    , lvttLength :: Word16
                                    , localVariableTypes :: [LocalVariableTypeTable] }
  | AttributeDeprecated { attributeHeader :: AttributeInfoHeader }
  | AttributeBootstrapMethods { attributeHeader      :: AttributeInfoHeader
                              , bootstrapMethodCount :: Word16
                              , bootstrapMethods     :: [BootstrapMethods] }
  | AttributeMethodParameters { attributeHeader       :: AttributeInfoHeader
                              , methodParametersCount :: Word16
                              , methodParametersInfo  :: [ParametersInfo] }
  | AttributeRuntimeAnnotations { attributeHeader    :: AttributeInfoHeader
                                , runtimeAnnotations :: ParameterAnnotation }
  | AttributeRuntimeParameterAnnotations { attributeHeader :: AttributeInfoHeader
                                         , runtimeParamAnnotationsCount :: Word8
                                         , runtimeParamAnnotations :: [ParameterAnnotation] }
  | AttributeRuntimeTypeAnnotations { attributeHeader :: AttributeInfoHeader
                                    , runtimeTypeAnnotationsCount :: Word8
                                    , runtimeTypeAnnotations :: [ParameterAnnotation] }
  | AttributeAnnotationDefault { attributeHeader        :: AttributeInfoHeader
                               , annotationDefaultValue :: CFElementValueT }

data ExceptionTable = ExceptionTable
  { exceptionStartPc   :: Word16
  , exceptionEndPc     :: Word16
  , exceptionHandlerPc :: Word16
  , exceptionCatchType :: Word16
  }

-- SMT: Stack Map Table
data SMTVerificationTypeInfo
  = TopVariableInfo { smtTypeTag :: Word8 }
  | IntegerVariableInfo { smtTypeTag :: Word8 }
  | FloatVariableInfo { smtTypeTag :: Word8 }
  | DoubleVariableInfo { smtTypeTag :: Word8 }
  | LongVariableInfo { smtTypeTag :: Word8 }
  | NullVariableInfo { smtTypeTag :: Word8 }
  | UninitializedThisVariableInfo { smtTypeTag :: Word8 }
  | ObjectVariableInfo { smtTypeTag                      :: Word8
                       , objectVariableConstantPoolIndex :: Word8 }
  | UninitializedVariableInfo { smtTypeTag                  :: Word8
                              , uninitializedVariableOffset :: Word8 }

data SMTFrame
  = SMTSameFrame { smtFrameType :: Word8 }
  | SMTSameLocal1Frame { smtFrameType :: Word8
                       , smtStack     :: [SMTVerificationTypeInfo] }
  | SMTSameLocal1FrameExt { smtFrameType   :: Word8
                          , smtOffsetDelta :: Word16
                          , smtStack       :: [SMTVerificationTypeInfo] }
  | SMTChopFrame { smtFrameType   :: Word8
                 , smtOffsetDelta :: Word16 }
  | SMTSameFrameExt { smtFrameType   :: Word8
                    , smtOffsetDelta :: Word16 }
  | SMTAppendFrame { smtFrameType   :: Word8
                   , smtOffsetDelta :: Word16
                   , smtLocals      :: [SMTVerificationTypeInfo] }
  | SMTFullFrame { smtFrameType   :: Word8
                 , smtOffsetDelta :: Word16
                 , smtLocalCount  :: Word16
                 , smtLocals      :: [SMTVerificationTypeInfo]
                 , smtStackCount  :: Word16
                 , smtStack       :: [SMTVerificationTypeInfo] }

data ClassesInfo = ClassesInfo
  { innerClassInfoIndex   :: Word16
  , outerClassInfoIndex   :: Word16
  , innerClassNameIndex   :: Word16
  , innerClassAccessFlags :: Word16
  }

data LineNumberTable = LineNumberTable
  { lineNumberTableStartPc :: Word16
  , lineNumberTableLineNo  :: Word16
  }

data LocalVariableTable = LocalVariableTable
  { localVariableTableStartPc         :: Word16
  , localVariableTableLength          :: Word16
  , localVariableTableNameIndex       :: Word16
  , localVariableTableDescriptorIndex :: Word16
  , localVariableTableIndex           :: Word16
  }

data LocalVariableTypeTable = LocalVariableTypeTable
  { localVariableTypeTableStartPc        :: Word16
  , localVariableTypeTableLength         :: Word16
  , localVariableTypeTableNameIndex      :: Word16
  , localVariableTypeTableSignatureIndex :: Word16
  , localVariableTypeTableIndex          :: Word16
  }

data BootstrapMethods = BootstrapMethods
  { bootstrapMethodRef  :: Word16
  , bootstrapMethodArgc :: Word16
  , bootstrapMethodArgv :: [Word16]
  }

data ParametersInfo = ParametersInfo
  { paramNameIndex   :: Word16
  , paramAccessFlags :: Word16
  }

data CFElementValueT = CFElementValueT
  { cfTag    :: Word8
  , cfValues :: [CFValue]
  }

data CFElementValuePairsT = CFElementValuePairsT
  { cfElementNameIndex :: Word16
  , cfElementValue     :: CFElementValueT
  }

data CFValue
  = CFConstValue { cfConstValueIndex :: Word16 }
  | CFEnumConstValue { cfTypeNameIndex  :: Word16
                     , cfConstNameIndex :: Word16 }
  | CFClassInfo { cfClassInfoIndex :: Word16 }
  | CFAnnotation { cfTypeIndex             :: Word16
                 , cfElementValuePairCount :: Word16
                 , cfElementValuePairs     :: [CFElementValuePairsT] }
  | CFArrayValue { cfValueCount    :: Word16
                 , cfElementValues :: [CFElementValueT] }

data AddressedT = AddressedT
  { addressedStartPc :: Word16
  , addressedLength  :: Word16
  , addressedIndex   :: Word16
  }

data PathT = PathT
  { typePathKind     :: Word8
  , typePathArgIndex :: Word8
  }

data TypePathT = TypePathT
  { typePathCount :: Word8
  , typePaths     :: [PathT]
  }

data AnnotationTargetInfo
  = TypeParameterTarget { typeParamIndex :: Word8 }
  | SuperTypeTarget { superTypeTargetIndex :: Word8 }
  | TypeParameterBoundTarget { typeParamIndex :: Word8
                             , boundType      :: Word8 }
  | EmptyTarget
  | FormalParameterTarget { formalParamIndex :: Word8 }
  | ThrowsTarget { throwsTypeIndex :: Word8 }
  | LocalVarTarget { localVarTargetAddressedLength :: Word16
                   , localVarAddressed             :: [AddressedT] }
  | CatchTarget { catchExceptionTableIndex :: Word16 }
  | OffsetTarget { offsetTargetOffset :: Word16 }
  | TypeArgumentTarget { typeArgOffset :: Word16
                       , typeArgIndex  :: Word8 }

data TypeAnnotation = TypeAnnotation
  { typeAnnotationTargetType :: Word8
  , typeAnnotationTargetInfo :: AnnotationTargetInfo
  , typeAnnotationTypePath   :: TypePathT
  , typeAnnotations          :: [CFValue]
  }

data ParameterAnnotation = ParameterAnnotation
  { paramAnnotationCount :: Word16
  , paramAnnotations     :: [CFValue]
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
  , classFileAttributes          :: [AttributeInfo]
  }
