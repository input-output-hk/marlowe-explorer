module Language.Marlowe.Runtime.AdjustTip
  ( BuildChain(..)
  , adjustTip
  , processTip

  -- re-exporting
  , Seq
  , (><)
  , empty
  , fromList
  , toList
  )
  where

import Data.Foldable ( toList )  -- Used in this module as the counterpart to Sequence.fromList
import Data.Monoid ( First(..), getFirst )
import Data.Sequence ( Seq, (><), drop, elemIndexL, empty, fromList, mapWithIndex, take )
import Prelude hiding ( drop, take )


{-
before

0..9  10..19  20..24..29  30..34,<24>,35..39
 t1     t2        t3             chain

after

0..24,35..39 
  newChain

So what are the cases?

1. We found the divergence point, added new tip and used rest of old chain.
   Chain is complete
2. We didn't find divergence point, added new tip to new chain and must keep
   looking. Chain is NOT COMPLETE
3. We ran out of new tip segments to search for and they've all been added.
   Chain is complete. (This represents the first time we run with an empty
   chain. Like server start-up)

1 and 3 seem like the same thing? (Just newChain)
-}


data BuildChain a
  = Building (Seq a)  -- Didn't find divergence point yet, need more tip segments
  | BuildDone (Seq a)  -- Found divergence point or old chain exhausted
  deriving (Eq, Show)


-- This function simulates the machinery of acquiring new tip segments and
-- trying each one against the existing chain to compute a new chain in total.
-- adjustTip isn't used by the Marlowe Explorer code but illustrates using
-- processTip below
adjustTip :: Eq a => Seq a -> [Seq a] -> Seq a
adjustTip _ [] = empty
adjustTip oldChain (tip : tips) = case processTip oldChain tip of
  BuildDone endOfChain -> endOfChain
  Building newTip -> newTip >< adjustTip oldChain tips


-- Compute if and how a new tip maps onto an existing chain
-- The state of this match is indicated through the BuildChain type
processTip :: Eq a => Seq a -> Seq a -> BuildChain a
processTip oldChain tip =
  -- We use the First Monoid to isolate the first location where the new tip and
  -- the old chain have a common element.
  case getFirst . mconcat . map First . toList $ mapWithIndex (maybeMatch oldChain) tip of
    -- This case means we've found the divergence point and can stop
    --   indexTip is the location on the new tip left of which is not yet on the chain
    --   chainTail is the portion of chain with the rollback tip discarded
    Just (indexTip, chainTail) -> BuildDone (take indexTip tip >< chainTail)

    -- This case means we didn't find the divergence point and must prepend the
    -- entire tip to the new chain. And signals to the caller of this function
    -- (through the Building constructor) that we must process more sequences
    -- from the tip.
    Nothing -> Building tip

  where
    -- Predicate for mapWithIndex above
    -- Try to find the given element in the sequence. If we do, evaluate to
    -- Just (Int, Seq a) where the Int is the index where the rollback overlap
    -- starts in the new tip and the Sequence is the old chain trimmed of the
    -- "bad" tip.
    -- These parts are used to fashion the new chain from the "good" tip and
    -- part of the old chain.
    maybeMatch :: Eq a => Seq a -> Int -> a -> Maybe (Int, Seq a)
    maybeMatch oldChain' indexTip elemTip =
      (\indexChain -> Just (indexTip, drop indexChain oldChain'))
        =<< elemIndexL elemTip oldChain'
