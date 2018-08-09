module Main where

import Prelude
import Effect (Effect)
import Effect.Class (liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Container as Container
import Game (newGame)

main :: Effect Unit
main = HA.runHalogenAff do
  game <- liftEffect newGame
  body <- HA.awaitBody
  runUI Container.component game body