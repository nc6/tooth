import Data.List (elemIndex, sort)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as UV

import Numeric.Combinations (combIndexSorted)

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Property

data CombInAlphabet = CombInAlphabet [Int] [Int]
  deriving Show

instance Arbitrary CombInAlphabet where
  arbitrary = do
    alphaSize <- choose (4, 50)
    let alpha = [0 .. alphaSize]
    k <- choose (1, min alphaSize 5)
    x <- vectorOf k (elements alpha)
    return $ CombInAlphabet alpha x

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations z (x:xs) = ((x:) <$> combinations (z-1) xs) ++ combinations z xs

prop_chooseIsSorted :: [Char] -> Int -> Property
prop_chooseIsSorted xs x = x > 1 ==> let
    choices = combinations x xs
  in
    choices == sort choices

prop_indexIsIndex :: CombInAlphabet -> Property
prop_indexIsIndex (CombInAlphabet alpha x) =
    (length x == Set.size (Set.fromList x)) ==>
  let
    x_sorted = sort x
    combs = combinations (length x) alpha
    mRealIdx = elemIndex x_sorted (sort combs)
    mCalculatedIdx = combIndexSorted (UV.fromList alpha) (UV.fromList x_sorted)
  in case (fromIntegral <$> mRealIdx) == mCalculatedIdx of
    True -> succeeded
    False -> let
        msg = "realIdx: " ++ show mRealIdx ++ " combIdx: " ++ show mCalculatedIdx
      in
        MkResult (Just False) True msg Nothing False Map.empty Set.empty []

main = quickCheck prop_indexIsIndex
-- main = let
--     alpha = UV.fromList ['a'..'e']
--     x = UV.fromList ['b','c']
--   in print $ combIndex alpha x
