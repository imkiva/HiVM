{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}

import           Utils.JavaString

testJavaString :: (JString a) => a -> IO ()
testJavaString = print . toJavaString

main :: IO ()
main = testJavaString "hello world"
