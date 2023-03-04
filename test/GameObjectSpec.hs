{-# LANGUAGE NoImplicitPrelude #-}

module GameObjectSpec (spec) where

import Control.Applicative (pure, (<$>), (<*>))
import GHC.Enum (Bounded (..), Enum (..))
import GameObject
import Orientation
import OrientationSpec hiding (spec)
import Relude (Bool, Eq (..), Hashable (..), Show (..), Word8, drop, foldMap, show, ($), (-))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (total)
import TestUtils

instance Arbitrary ObjectType where
  arbitrary = oneof [pure Source, pure Mirror]
  shrink Source = []
  shrink Mirror = [Source]

instance Arbitrary GameObject where
  arbitrary = GameObject <$> arbitrary <*> arbitrary <*> arbitrary
  shrink go@(GameObject (x, y) o s) = drop 1 $
    do
      x' <- x : shrink x
      y' <- y : shrink y
      o' <- o : shrink o
      s' <- s : shrink s
      pure $ GameObject (x', y') o' s'

spec :: Spec
spec = do
  describe "GameObjectType" $ do
    it "is an Enum" $ do
      enumFromTo Source Mirror `shouldBe` [Source, Mirror]
    it "has an Arbitrary instance" $ do
      property $ \x -> total (x :: ObjectType)
    it "has a Show instance" $ do
      show Source `shouldBe` "Source"
      show Mirror `shouldBe` "Mirror"
    it "has a terminating shrink" $ do
      property $ \x -> shrinkLoop 3 (x :: ObjectType)
    it "has a Bounded instance" $ do
      minBound `shouldBe` Source
      maxBound `shouldBe` Mirror
  describe "GameObject" $ do
    it "has an Arbitrary instance" $ do
      property $ \x -> total (x :: GameObject)
    it "has a Show instance" $ do
      show (GameObject (0, 0) East Source) `shouldBe` "GameObject {_position = (0.0,0.0), _orientation = East, _shape = Source}"
    it "has a Hashable instance" $ do
      hashWithSalt 0 (GameObject (0, 0) East Source) `shouldBe` 0
    it "has a terminating shrink" $ do
      property $ \x -> shrinkLoop 1 (x :: GameObject)

testObject = GameObject (0, 0) East Source
