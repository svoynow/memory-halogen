module Animal (Animal, toString, url, all) where 

import Prelude

import Data.List (fromFoldable) as L
import Data.List (List)
  
data Animal =
    Badger
  | Bee
  | Bird
  | Butterfly
  | Cat
  | Chicken
  | Cow
  | Crab
  | Deer
  | Dinosaur
  | Dog
  | Dolphin
  | Elephant
  | Duck
  | Frog
  | Gorilla
  | Horse
  | Kangaroo
  | Leopard
  | Lion
  | Llama
  | Octopus
  | Rhinoceros
  | Shark
  | Sheep
  | Snail
  | Turtle
  | Whale
  | Wolf

derive instance eqAnimal :: Eq Animal

all :: List Animal
all = L.fromFoldable [
  Badger,
  Bee,
  Bird,
  Butterfly,
  Cat,
  Chicken,
  Cow,
  Crab,
  Deer,
  Dinosaur,
  Dog,
  Dolphin,
  Elephant,
  Duck,
  Frog,
  Gorilla,
  Horse,
  Kangaroo,
  Leopard,
  Lion,
  Llama,
  Octopus,
  Rhinoceros,
  Shark,
  Sheep,
  Snail,
  Turtle,
  Whale,
  Wolf
]

toString :: Animal -> String
toString a = 
  case a of
     Badger -> "Badger"
     Bee -> "Bee"
     Bird -> "Bird"
     Butterfly -> "Butterfly"
     Cat -> "Cat"
     Chicken -> "Chicken"
     Cow -> "Cow"
     Crab -> "Crab"
     Deer -> "Deer"
     Dinosaur -> "Dinosaur"
     Dog -> "Dog"
     Dolphin -> "Dolphin"
     Elephant -> "Elephant"
     Duck -> "Duck"
     Frog -> "Frog"
     Gorilla -> "Gorilla"
     Horse -> "Horse"
     Kangaroo -> "Kangaroo"
     Leopard -> "Leopard"
     Lion -> "Lion"
     Llama -> "Llama"
     Octopus -> "Octopus"
     Rhinoceros -> "Rhinoceros"
     Shark -> "Shark"
     Sheep -> "Sheep"
     Snail -> "Snail"
     Turtle -> "Turtle"
     Whale -> "Whale"
     Wolf -> "Wolf"

url :: Animal -> String
url a = (toString a) <> ".svg"
