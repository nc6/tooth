
-- https://drive.google.com/open?id=0BxJP-hCBgo5OcU9td2Fhc2xHek0

module Numeric.Combinations
  ( combIndex
  , combIndexSorted
  )
  where

import Control.Monad.ST (runST)

import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as UV
import Data.Vector.Algorithms.Merge (sort)

import Numeric.SpecFunctions (choose)

import Debug.Trace

-- | Calculates the combination index. Assumes that neither alpha nor x
--   contain any duplicate elements.
combIndex :: (UV.Unbox a, Ord a)
          => UV.Vector a -- Alphabet of length N
          -> UV.Vector a -- element of length K
          -> Maybe Int -- Index in C_K^A
combIndex alpha x | UV.length x > UV.length alpha = Nothing -- Require that the alphabet is bigger than the element!
combIndex alpha x = combIndexSorted alpha_sorted x_sorted where
  alpha_sorted = runST $ do
    m_alpha <- UV.thaw alpha
    sort m_alpha
    UV.unsafeFreeze m_alpha
  x_sorted = runST $ do
    m_x <- UV.thaw x
    sort m_x
    UV.unsafeFreeze m_x

-- | Calculates the combination index assuming both alpha and x are pre-sorted.
--   This will give incorrect answers if the inputs are not sorted!
combIndexSorted :: (UV.Unbox a, Ord a)
                => UV.Vector a -- Alphabet of length N
                -> UV.Vector a -- element of length K
                -> Maybe Int -- Index in C_K^A
combIndexSorted alpha x | UV.length x > UV.length alpha = Nothing -- Require that the alphabet is bigger than the element!
combIndexSorted alpha x = do
    x_idx <- m_x_idx
    return $ UV.sum
           . UV.map (\k -> inner_sum k x_idx)
           $ UV.enumFromN 0 cap_k
  where
    m_x_idx = UV.mapM (flip UV.elemIndex alpha) x
    cap_k = UV.length x
    cap_n = UV.length alpha
    i_k (-1) _ = -1
    i_k k x_idx = x_idx ! k
    inner_sum k x_idx =
          UV.sum
        . UV.map (\q -> floor
                      $ choose (cap_n - (i1 + 1) - q) (cap_k - k - 1))
        $ UV.enumFromN 1 (i0 - (i1 + 1))
      where
        i0 = i_k k x_idx
        i1 = i_k (k-1) x_idx
