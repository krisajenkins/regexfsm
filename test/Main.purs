module Test.Main where

import Prelude
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Main (match, simpleRegex)
import Test.Unit (suite, test)
import Test.Unit.Assert (assert, assertFalse)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main :: forall e. Eff (avar :: AVAR, testOutput :: TESTOUTPUT, console :: CONSOLE | e) Unit
main = runTest do
  suite "Main" do
    test "foo" do
      assert "Simple" $ match simpleRegex "b"
      assert "Harder" $ match simpleRegex "aaaaab"
      assertFalse "Bad" $ match simpleRegex "aacaab"
