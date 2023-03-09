{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TestUtils where

import Control.Lens
import Data.Word
import Relude
import Test.Hspec
import Test.QuickCheck

-- make sure that shrink terminates
shrinkLoop :: forall a. Arbitrary a => Word8 -> a -> Expectation
shrinkLoop 0 x = expectationFailure "shrinkLoop: infinite loop"
shrinkLoop n x = foldMap (shrinkLoop (n - 1)) (shrink x)

total :: forall a b. (Show a, Eq a) => a -> Expectation
total a = a `shouldBe` a

-- Prism laws:
prismLaw1 :: forall a b. (Eq a, Show a, Eq b, Show b) => Prism' a b -> b -> Expectation
prismLaw1 p b = preview p (review p b) `shouldBe` Just b

prismLaw2 :: forall a b. (Eq a, Show a, Eq b, Show b) => Prism' a b -> a -> Expectation
prismLaw2 p a =
  case preview p a of
    Nothing -> pure ()
    Just y -> review p y `shouldBe` a

prismLaw3 :: forall a b. (Eq a, Show a, Eq b, Show b) => Prism' a b -> a -> Expectation
prismLaw3 p a =
  case matching p a of
    Left t -> matching p t `shouldBe` Left a
    _ -> pure ()

isPrism :: forall a b. (Eq a, Show a, Eq b, Show b, Arbitrary a, Arbitrary b) => String -> Prism' a b -> Spec
isPrism description thePrism =
  describe ("has a prism for " <> description) $ do
    it "satifies the first prism law" $ do
      property $ prismLaw1 (thePrism :: Prism' a b)
    it "satifies the second prism law" $ do
      property $ prismLaw2 (thePrism :: Prism' a b)
    it "satifies the third prism law" $ do
      property $ prismLaw3 (thePrism :: Prism' a b)

-- lens laws:
lensLaw1 :: forall a b. (Eq a, Show a, Eq b, Show b) => Lens' a b -> a -> Expectation
lensLaw1 l a = view l (set l (view l a) a) `shouldBe` view l a

lensLaw2 :: forall a b. (Eq a, Show a, Eq b, Show b) => Lens' a b -> a -> b -> Expectation
lensLaw2 l a b = set l b (set l b a) `shouldBe` set l b a

isLens :: forall a b. (Eq a, Show a, Eq b, Show b, Arbitrary a, Arbitrary b) => String -> Lens' a b -> Spec
isLens description theLens =
  describe ("has a lens for" <> description <> ":") $ do
    it "satifies the first lens law" $ do
      property $ \(x : a) -> lensLaw1 (theLens :: Lens' a b) x
    it "satifies the second lens law" $ do
      property $ \(x : a) (y : b) -> lensLaw2 (theLens :: Lens' a b) x y

(~=) :: Float -> Float -> Expectation
a ~= b =
  if abs (a - b) < 0.001
    then pure ()
    else a `shouldBe` b
