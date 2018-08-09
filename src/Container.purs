module Container (Query, component) where

import Prelude
import Data.Array as A
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Game (Action(..), doAction, hasMatch) as Game
import Game (Game, TurnState(..))
import Tableau (Item, image, itemToString, pairsCount) as Tableau


data Query a
   = GameAction (Game.Action Tableau.Item) a
  
type State = Game
 
component :: H.Component HH.HTML Query Game Void Aff
component =
  H.component
    { initialState: identity
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div [HP.class_ $ ClassName "app"]
     [HH.div [HP.class_ $ ClassName "grid"] 
      (renderCards state.tableau <> renderMessage state <> renderResults state)
      ]

  renderCards t = map renderCard $ A.fromFoldable $ t

  renderCard item = 
    HH.img
      [
        HP.class_ $ ClassName "card-image"
      , HP.src $ Tableau.image item
      , HE.onClick (HE.input_ (GameAction $ Game.SelectCard item))
      ] 

  renderResults state = 
    [HH.div [HP.class_ $ ClassName "result"] [HH.text results]]
    where 
      count = Tableau.pairsCount state.tableau
      inflected = if count == 1 then "pair" else "pairs"
      results = (show count) <> " " <> inflected <> " found"

  renderMessage game@{turnState} = 
    [HH.div [HP.class_ $ ClassName "message"] [contents]]
    where 
      contents = 
        case turnState of
          NotStarted -> HH.text $ message turnState
          OneCardFlipped a -> HH.text $ message turnState
          TwoCardsFlipped a b -> button turnState Game.Continue
          GameComplete -> button turnState Game.Reset  
      
  button t m = 
    HH.button [HE.onClick $ HE.input_ (GameAction m)] [HH.text $ message t]

  message NotStarted = "Choose a card"
  message (OneCardFlipped card) = "Try to find the other " <> Tableau.itemToString card
  message state@(TwoCardsFlipped _ _) | Game.hasMatch state = "A match! Click to continue"
  message (TwoCardsFlipped _ _) = "No match. Reset and try again"
  message GameComplete = "All done! Play again?"

  eval :: Query ~> H.ComponentDSL State Query Void Aff
  eval = case _ of
    GameAction action next -> do
      gameState <- H.get
      newGameState <- H.liftEffect $ Game.doAction action gameState
      H.modify_ $ \g -> either (const g) (identity) newGameState      
      pure next