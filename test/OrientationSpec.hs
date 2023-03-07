{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fprof-auto-calls #-}

module OrientationSpec (spec) where

import Control.Lens
import Graphics.Gloss (Point)
import Orientation
import Relude
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (total)
import TestUtils
import Utils
import Graphics.Gloss.Data.Point.Arithmetic qualified as G
import Text.Printf (printf)

spec :: Spec
spec = do
  describe "Orientation" $ do
    it "has an Arbitrary instance" $ do
      property $ \x -> total (x :: Orientation)
    it "has a Show instance" $ do
      showT East `shouldBe` "East"
      showT West `shouldBe` "West"
      showT North `shouldBe` "North"
      showT South `shouldBe` "South"
    it "has a Hashable instance" $ do
      hashWithSalt 0 East `shouldBe` 0
      hashWithSalt 0 West `shouldBe` 4398046512844
      hashWithSalt 0 North `shouldBe` 2199023256422
      hashWithSalt 0 South `shouldBe` 6597069769266
    it "has a terminating shrink" $ do
      property $ \x -> shrinkLoop 100 (x :: Orientation)
  describe "normalizeRadians" $ do
    it "normalizes an angle to the range (-pi, pi]" $ do
      normalizeRadians 0 ~= 0
      normalizeRadians 1 ~= 1
      normalizeRadians (-1) ~= (-1)
      normalizeRadians (2 * pi) ~= 0
      normalizeRadians (2 * pi + 1) ~= 1
      normalizeRadians (2 * pi - 1) ~= (-1)
      normalizeRadians (-2 * pi) ~= 0
      normalizeRadians (-2 * pi + 1) ~= 1
      normalizeRadians (-2 * pi - 1) ~= (-1)
      normalizeRadians (3 * pi) ~= pi
      normalizeRadians (3 * pi + 1) ~= (1 - pi)
      normalizeRadians (3 * pi - 1) ~= (pi - 1)
      normalizeRadians (-3 * pi) ~= (-pi)
      normalizeRadians (-3 * pi + 1) ~= (1 - pi)
      normalizeRadians (-3 * pi - 1) ~= (pi - 1)
    it "normalizes an angle to the range (-pi, pi] for random inputs" $ do
      property $ \x -> normalizeRadians x >= (-pi) && normalizeRadians x <= pi
    it "is total" $ do
      property $ \x -> total (x :: Double)
  describe "inOriOf" $ do
    it "returns LT if we move in the direction of the orientation" $ do
      property $ \ ori (x :: Int16) (y :: int16) -> 
        let p = (denormalize x, denormalize y)
        in inOriOf ori p (p G.+ toSquarePoint ori) `shouldBe` LT
    it "returns GT if we move in the opposite direction of the orientation" $ do
      property $ \ ori (x :: Int16) (y :: int16) -> 
        let p = (denormalize x, denormalize y)
        in inOriOf ori (p G.+ toSquarePoint ori) p `shouldBe` GT
    it "returns EQ if we in a right angle to the orientation" $ do
      property $ \ ori (x :: Int16) (y :: int16) -> 
        let p = (denormalize x, denormalize y)
            q = rotatePiOver2 (toSquarePoint ori)
            result = inOriOf ori p (p G.+ q) 
        in if result == EQ
          then pure ()
          else expectationFailure (printf "inOriOf %s %s %s = %s expecting %s" (showT ori) (showT p) (showT (p G.+ q)) (showT result) (showT EQ))

instance Arbitrary Orientation where
  arbitrary = oneof [pure East, pure West, pure North, pure South]
  shrink x = case x of
    East -> []
    West -> [East]
    North -> [East, West]
    South -> [East, West, North]
