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
type Name = String

data Select = Select Name Items

type State =
  { built :: Maybe Select
  , form ::
    { item :: Item
    , name :: String
    }
  , items :: Items
  }

data Action
  = Noop
  | AddItem
  | BuildForm
  | EditItem String
  | EditName String

component :: Component Props
component = createComponent "App"

app :: JSX
app = make component { initialState, render, update } {}

initialState :: State
initialState =
  { built: Nothing
  , form:
    { item: ""
    , name: ""
    }
  , items: []
  }

render :: Self Props State Action -> JSX
render self@{ state: { built, form, items } } =
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
            [ H.span_ [ H.text "name" ]
            , H.input
              { onChange:
                  capture
                    self
                    targetValue
                    (\v -> EditName (fromMaybe "" v))
              , value: form.name
              }
            ]
          , H.br {}
          , H.label_
            [ H.span_ [ H.text "item" ]
            , H.input
              { onChange:
                  capture
                    self
                    targetValue
                    (\v -> EditItem (fromMaybe "" v))
              , value: form.item
              }
            ]
          , H.br {}
          , H.button
            { onClick:
                capture
                  self
                  preventDefault
                  (const AddItem)
            , children: [ H.text "Add" ]
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
              Just (Select name items') ->
                H.label_
                [ H.span_ [ H.text name ]
                , H.select
                  { name
                  , children:
                      mapFlipped
                        items'
                        (\i' -> H.option_ [ H.text i' ])
                  }
                ]
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
  Update state { form = state.form { item = "" }, items = Array.snoc state.items state.form.item }
update self@{ state } BuildForm =
  Update state { built = Just (Select state.form.name state.items), form = { item: "", name: "" } }
update self@{ state } (EditItem v) =
  Update state { form = state.form { item = v } }
update self@{ state } (EditName v) =
  Update state { form = state.form { name = v } }
