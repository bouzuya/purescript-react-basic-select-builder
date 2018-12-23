module Component.App
  ( app
  ) where

import Prelude

import Data.Array as Array
import Data.Functor (mapFlipped)
import Data.Maybe (Maybe(..), fromMaybe)
import React.Basic (Component, JSX, Self, StateUpdate(..), capture, createComponent, make)
import React.Basic.DOM as H
import React.Basic.DOM.Events (preventDefault, targetValue)

type Props =
  {}

type Item = String
type Items = Array Item

type State =
  { built :: Maybe Items
  , item :: Item
  , items :: Items
  }

data Action
  = Noop
  | AddItem
  | BuildForm
  | EditItem String

component :: Component Props
component = createComponent "App"

app :: JSX
app = make component { initialState, render, update } {}

initialState :: State
initialState =
  { built: Nothing
  , item: ""
  , items: []
  }

render :: Self Props State Action -> JSX
render self@{ state: { built, item, items } } =
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
        , H.div_
          [ H.button
            { onClick:
                capture
                  self
                  preventDefault
                  (const BuildForm)
            , children: [ H.text "Build" ]
            }
          ]
        , H.div_
          [ case built of
              Nothing -> H.text "Not build"
              Just items' ->
                H.select_
                  (mapFlipped
                    items'
                    (\i' -> H.option_ [ H.text i' ] ))
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
update self@{ state } BuildForm =
  Update state { built = Just state.items }
update self@{ state } (EditItem v) =
  Update state { item = v }
