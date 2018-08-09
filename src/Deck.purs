module Deck (Deck, deal) where
  
import Prelude
import Data.List (List)
import Data.List as L
import Data.List.Lazy as LZ
import Data.Tuple (snd, Tuple(..))
import Effect (Effect)
import Effect.Random (random)

import Animal (all) as Animal
import Animal (Animal)
import Card (Card)
import Card (makePair) as Card

type Deck = L.List Card

shuffle :: forall a. L.List a -> Effect (L.List a)
shuffle xs = do
  randoms <- LZ.replicateM (L.length xs) random  
  pure $ map snd $ L.sortBy compareFst $ L.zip (LZ.toUnfoldable randoms) xs
  where compareFst (Tuple a _) (Tuple b _) = compare a b

randomAnimals :: Int -> Effect (List Animal)
randomAnimals num = L.take num <$> (shuffle Animal.all)

deal :: Int -> Effect Deck
deal pairs = do
  animals <- randomAnimals pairs
  shuffle $ L.concatMap Card.makePair animals  
  
