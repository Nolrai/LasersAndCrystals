module OrientationSpec (spec) where

import Control.Lens
import Graphics.Gloss (Point)
import Orientation
import Relude (hashWithSalt)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (total)
import TestUtils

spec :: Spec
spec = do
  focus $
    describe "Orientation" $
      isPrism "degrees" (asDegrees :: Prism' Float Orientation)
        >> isPrism "radians" (asRadians :: Prism' Float Orientation)
        >> isPrism "square points" (asSquarePoint :: Prism' Point Orientation)
        >> isPrism "unit points" (asSquarePoint :: Prism' Point Orientation)
        >> do
          it "has an Arbitrary instance" $ do
            property $ \x -> total (x :: Orientation)
          it "has a Show instance" $ do
            show East `shouldBe` "East"
            show West `shouldBe` "West"
            show North `shouldBe` "North"
            show South `shouldBe` "South"
          it "has a Hashable instance" $ do
            hashWithSalt 0 East `shouldBe` 0
            hashWithSalt 0 West `shouldBe` 4398046512844
            hashWithSalt 0 North `shouldBe` 2199023256422
            hashWithSalt 0 South `shouldBe` 6597069769266
          it "has a terminating shrink" $ do
            property $ \x -> shrinkLoop 100 (x :: Orientation)

instance Arbitrary Orientation where
  arbitrary = oneof [pure East, pure West, pure North, pure South]
  shrink x = case x of
    East -> []
    West -> [East]
    North -> [East, West]
    South -> [East, West, North]
