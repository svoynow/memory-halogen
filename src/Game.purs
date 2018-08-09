module Game (
  Action(..),
  TurnState(..),
  Game,
  doAction,
  hasMatch,
  newGame
) where

import Prelude

import Data.Either (Either(..))
import Effect(Effect)  

import Deck (deal) as Deck
import Deck (Deck)
import Tableau (Item, isMatch, make, flipCard, pairCards, complete, reset) as Tableau
import Tableau (Tableau, CardState(..))

data TurnState a = 
    NotStarted
  | OneCardFlipped a
  | TwoCardsFlipped a a
  | GameComplete

data Action a = 
    SelectCard a
  | Continue
  | Reset


type Game = {
  tableau :: Tableau,
  turnState :: TurnState Tableau.Item
}

initialState :: Deck -> Game
initialState deck = { tableau: Tableau.make deck, turnState: NotStarted }

newGame :: Effect Game
newGame = Deck.deal 8 >>= (pure <<< initialState)

hasMatch :: TurnState Tableau.Item -> Boolean
hasMatch state = 
  case state of
    TwoCardsFlipped x y -> Tableau.isMatch x y
    _ -> false

doAction :: Action Tableau.Item -> Game -> Effect (Either String Game)
doAction (SelectCard item@{ card, cardState: FaceDown }) { tableau, turnState : NotStarted } = 
  pure $ Right $ { 
    tableau: Tableau.flipCard item tableau, 
    turnState: (OneCardFlipped item) 
  }

doAction (SelectCard item@{ card, cardState: FaceDown }) { tableau, turnState : (OneCardFlipped flipped) } = 
  pure $ Right $ {
     tableau: Tableau.flipCard item tableau, 
     turnState: (TwoCardsFlipped item flipped) 
  }

doAction Continue { tableau, turnState : ts@(TwoCardsFlipped a b) } | hasMatch ts =
  pure $ Right $ {
     tableau: tableau',
     turnState: if Tableau.complete(tableau') then GameComplete else NotStarted
  }  
  where tableau' = Tableau.pairCards [a, b] tableau

doAction Continue { tableau, turnState : ts@(TwoCardsFlipped a b) } | not hasMatch ts =
  pure $ Right $ {
     tableau: Tableau.reset [a, b] tableau,
     turnState: NotStarted
  }  
  
doAction Reset _ = map Right newGame

doAction _ _ = pure $ Left "Nope"
 

  

