{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}

module Utils.JavaString where

import           Data.Text (Text, pack)

newtype JavaString =
  JavaString Text
  deriving (Show)

class JString a where
  toJavaString :: a -> JavaString

instance JString [Char] where
  toJavaString = toJavaString . pack

instance JString Text where
  toJavaString = JavaString

toText :: JavaString -> Text
toText (JavaString t) = t
