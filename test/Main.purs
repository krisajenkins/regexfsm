module Test.Main (main) where

import Prelude
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Main (match, regex1, regex2)
import Test.Unit (suite, test)
import Test.Unit.Assert (assert, assertFalse)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main :: forall e. Eff (avar :: AVAR, testOutput :: TESTOUTPUT, console :: CONSOLE | e) Unit
main = runTest do
  suite "Main" do
    test "a*b" do
      assert "Simple" $ match regex1 "b"
      assert "Harder" $ match regex1 "aaaaab"
      assertFalse "Bad" $ match regex1 "c"
      assertFalse "Bad" $ match regex1 "aacaab"
    test "(ab|ac)+" do
      assert "Simple" $ match regex2 "ab"
      assert "Simple" $ match regex2 "ac"
      assert "Harder" $ match regex2 "abacabac"
      assertFalse "Bad" $ match regex2 "c"
      assertFalse "Bad" $ match regex2 "aacaab"
