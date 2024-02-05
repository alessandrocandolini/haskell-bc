module GrammarSpec where

import Parser
import Grammar
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property

spec :: Spec
spec = describe "Simple test" $ do

     it "parse literal number" $
       parseAll expr "1234.34" == Just (Literal 1234.34)

     it "parse add" $
       parseAll expr "(1+3)" == Just (Add (Literal 1) (Literal 3))


     prop "property-based unit test" $
        \l -> reverse ( reverse l ) == ( l::[Int])
