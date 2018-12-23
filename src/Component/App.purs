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

type Form =
  { item :: Item
  , items :: Items
  , label :: String
  , name :: String
  }
type Item = String
type Items = Array Item
type Label = String
type Name = String

data Select = Select Name Label Items

type State =
  { built :: Maybe Select
  , form :: Form
  }

data Action
  = Noop
  | AddItem
  | BuildForm
  | EditItem String
  | EditLabel String
  | EditName String

component :: Component Props
component = createComponent "App"

app :: JSX
app = make component { initialState, render, update } {}

buildFromForm :: Form -> Select
buildFromForm { name, items, label } = Select name label items

initialForm :: Form
initialForm = { item: "", items: [], label: "", name: "" }

initialState :: State
initialState =
  { built: Nothing
  , form: initialForm
  }

render :: Self Props State Action -> JSX
render self@{ state: { built, form } } =
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
            [ H.span_ [ H.text "label" ]
            , H.input
              { onChange:
                  capture
                    self
                    targetValue
                    (\v -> EditLabel (fromMaybe "" v))
              , value: form.label
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
              form.items
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
              Just (Select name label items') ->
                H.label_
                [ H.span_ [ H.text label ]
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
  Update
    state
      { form = state.form { item = "", items = Array.snoc state.form.items state.form.item } }
update self@{ state } BuildForm =
  Update state { built = Just (buildFromForm state.form), form = initialForm }
update self@{ state } (EditItem v) =
  Update state { form = state.form { item = v } }
update self@{ state } (EditLabel v) =
  Update state { form = state.form { label = v } }
update self@{ state } (EditName v) =
  Update state { form = state.form { name = v } }
