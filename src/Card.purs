module Card (
  Card, 
  Group, 
  CardKey, 
  makePair, 
  isMatch, 
  isSameCard, 
  imageUrl,
  key
  ) where

import Prelude
import Data.List (List)
import Data.List (fromFoldable) as L

import Animal (Animal)
import Animal as A

data Group = A | B

derive instance eqGroup :: Eq Group

type Card = {
    animal :: Animal
  , group :: Group
}

newtype CardKey = CardKey String
derive newtype instance eqCardKey :: Eq CardKey
derive newtype instance ordCardKey :: Ord CardKey

groupString :: Group -> String
groupString A = "A"
groupString B = "B"

initialize :: Animal -> Group -> Card
initialize a g = { animal: a, group: g}

makePair :: Animal -> List Card
makePair a = L.fromFoldable [initialize a A, initialize a B]

key :: Card -> CardKey
key { animal, group} = CardKey $ A.toString(animal) <> groupString group

imageUrl :: Card -> String
imageUrl { animal } = A.url(animal)

isMatch ::  Card -> Card -> Boolean
isMatch { animal: this } { animal: that} = this == that

isSameCard :: Card -> Card -> Boolean
isSameCard a@{ group: this } b@{ group: that} = (isMatch a b) && (this == that)