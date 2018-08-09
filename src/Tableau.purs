
module Tableau (
  Item,
  Tableau,
  CardState(..),
  image,
  itemToString,
  isMatch,
  make,
  flipCard,
  pairCards,
  pairsCount,
  complete,
  reset
  ) where
  
import Prelude
import Data.Foldable (class Foldable, foldl)
import Data.List (filter, length, all, head) as L
import Data.List (List)
import Data.Maybe (Maybe)
 
import Animal (toString) as Animal
import Card (Card, CardKey)
import Card (isMatch, isSameCard, imageUrl, key) as Card
import Deck (Deck)

data CardState = 
    FaceUp
  | FaceDown
  | Paired

derive instance eqCardState :: Eq CardState


type Item = {
  card :: Card,
  cardState :: CardState
}

type Tableau = List Item

changeItem :: (Item -> Item) -> Item -> Tableau -> Tableau
changeItem f i t = map fn t 
  where fn x = if (isSame x i) then f x else x

getItemState :: Tableau -> Item -> Maybe CardState
getItemState t i = (_.cardState) <$> (L.head $ L.filter (isSame i) t)

key :: Item -> CardKey
key { card } = Card.key card

itemToString :: Item -> String
itemToString item = Animal.toString item.card.animal

isMatch :: Item -> Item -> Boolean
isMatch { card: this } { card: that } = Card.isMatch this that

isSame :: Item -> Item -> Boolean
isSame { card: this } { card: that } = Card.isSameCard this that

image :: Item -> String
image { cardState, card} = 
  case cardState of
    FaceUp -> Card.imageUrl card
    FaceDown -> "blue.svg"
    Paired -> "x.png"

pairsCount :: Tableau -> Int
pairsCount t =  (L.length $ L.filter (\{ cardState } -> cardState == Paired) t) / 2

complete :: Tableau -> Boolean
complete t = L.all (\{ cardState } -> cardState == Paired) t

initializeItem :: Card -> Item
initializeItem c = { card: c, cardState: FaceDown }

changeItemState :: CardState -> Item -> Tableau -> Tableau
changeItemState s = changeItem (_ { cardState = s })

changeItemsState :: forall f. Foldable f => CardState -> f Item -> Tableau -> Tableau
changeItemsState s items t = foldl (\acc x -> changeItemState s x acc) t items

flipCard :: Item -> Tableau -> Tableau
flipCard = changeItemState FaceUp

pairCards :: forall f. Foldable f  => f Item -> Tableau -> Tableau
pairCards = changeItemsState Paired

make :: Deck -> Tableau
make = map initializeItem

reset :: forall f. Foldable f  => f Item -> Tableau -> Tableau
reset = changeItemsState FaceDown

