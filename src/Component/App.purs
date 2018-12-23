module Component.App
  ( app
  ) where

import Prelude

import Data.Array as Array
import Data.Functor (mapFlipped)
import Data.Maybe (fromMaybe)
import React.Basic (Component, JSX, Self, StateUpdate(..), capture, createComponent, make)
import React.Basic.DOM as H
import React.Basic.DOM.Events (preventDefault, targetValue)

type Props =
  {}

type State =
  { item :: String
  , items :: Array String
  }

data Action
  = Noop
  | AddItem
  | EditItem String

component :: Component Props
component = createComponent "App"

app :: JSX
app = make component { initialState, render, update } {}

initialState :: State
initialState =
  { item: ""
  , items: []
  }

render :: Self Props State Action -> JSX
render self@{ state: { item, items } } =
  H.div
  { className: "app"
  , children:
    [ H.div
      { className: "header"
      , children:
        [ H.h1_
          [ H.text "App" ]
        ]
      }
    , H.div
      { className: "body"
      , children:
        [ H.div_
          [ H.label_
            [ H.span_ [ H.text "item" ]
            , H.input
              { onChange:
                  capture
                    self
                    targetValue
                    (\v -> EditItem (fromMaybe "" v))
              , value: item
              }
            ]
          , H.br {}
          , H.button
            { onClick:
                capture
                  self
                  preventDefault
                  (const AddItem)
            , children: [ H.text "OK" ]
            }
          ]
        , H.div_
          [ H.ul_
            (mapFlipped
              items
              (\i ->
                H.li_
                [ H.text i
                ]
              )
            )
          ]
        ]
      }
    , H.div
      { className: "footer" }
    ]
  }

update :: Self Props State Action -> Action -> StateUpdate Props State Action
update self Noop = NoUpdate
update self@{ state } AddItem =
  Update state { item = "", items = Array.snoc state.items state.item }
update self@{ state } (EditItem v) =
  Update state { item = v }

