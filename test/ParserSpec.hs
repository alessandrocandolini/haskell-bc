module ParserSpec where

import Parser
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property

spec :: Spec
spec = describe "Simple test" $ do

     prop "char" $
      \c ->  parseAll (char c) [c] == Just c

     it "decimal" $
       parseAll decimal "1234.34" == Just 1234.34

     it "decimal" $
       parseAll (parenthesised decimal) "(1234.34)" == Just 1234.34


     prop "property-based unit test" $
        \l -> reverse ( reverse l ) == ( l::[Int])
