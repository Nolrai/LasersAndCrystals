{-# LANGUAGE NoImplicitPrelude #-}

module GameObjectSpec (spec) where

import Control.Applicative (pure, (<$>), (<*>))
import GHC.Enum (Bounded (..), Enum (..))
import GameObject
import Orientation
import OrientationSpec hiding (spec)
import Relude (Bool, Eq (..), Hashable (..), Show (..), Word8, show, ($), (-), concatMap, Int, foldMap, fst, snd)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (total, shuffle)
import TestUtils
import System.Random (mkStdGen, randoms)
import Data.List (zip, sortOn, drop, take, map)
import Control.Category ((.))

instance Arbitrary ObjectType where
  arbitrary = oneof [pure Source, pure Mirror]
  shrink Source = []
  shrink Mirror = [Source]

instance Arbitrary GameObject where
  arbitrary = GameObject <$> arbitrary <*> arbitrary <*> arbitrary
  shrink go@(GameObject (x, y) o s) = take 3 $ shuffle (hashWithSalt 7 go) $ drop 1 $
    do
      x' <- x : shrink x
      y' <- y : shrink y
      o' <- o : shrink o
      s' <- s : shrink s
      pure $ GameObject (x', y') o' s'

shuffle :: Int -> [a] -> [a]
shuffle seed = map snd . sortOn fst . withRandomKey
  where
    withRandomKey :: [a] -> [(Int, a)]
    withRandomKey = zip (randoms (mkStdGen seed) :: [Int])

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


testObject = GameObject (0, 0) East Source

flatShrink x = x : concatMap flatShrink (shrink x)
