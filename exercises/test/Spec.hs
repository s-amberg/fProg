import Test.Hspec

import qualified Chapter_01_Spec
import qualified Chapter_02_Spec
import qualified Chapter_03_Spec
import qualified Chapter_04_Spec
import qualified Chapter_05_Spec
import qualified Chapter_06_Spec
import qualified Chapter_07_Spec
import qualified Chapter_08_Spec
import qualified Chapter_12_Spec
import qualified BinarySearchTree_Spec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do 
  describe "Chapter_01"     Chapter_01_Spec.spec
  describe "Chapter_02"     Chapter_02_Spec.spec
  describe "Chapter_03"     Chapter_03_Spec.spec
  describe "Chapter_04"     Chapter_04_Spec.spec
  describe "Chapter_05"     Chapter_05_Spec.spec
  describe "Chapter_06"     Chapter_06_Spec.spec
  describe "Chapter_07"     Chapter_07_Spec.spec
  describe "Chapter_08"     Chapter_08_Spec.spec
  -- describe "Chapter_12"     Chapter_12_Spec.spec
  describe "BinarySearchTree"     BinarySearchTree_Spec.spec