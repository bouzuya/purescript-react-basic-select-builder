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
  { label :: String
  , name :: String
  , optionLabel :: String
  , optionValue :: String
  , options :: Array Option
  }
type OptionLabel = String
type OptionValue = String
data Option = Option OptionLabel OptionValue
type Label = String
type Name = String
data Select = Select Name Label (Array Option)

type State =
  { built :: Maybe Select
  , form :: Form
  }

data Action
  = Noop
  | AddOption
  | BuildForm
  | EditLabel String
  | EditName String
  | EditOptionLabel String
  | EditOptionValue String

component :: Component Props
component = createComponent "App"

app :: JSX
app = make component { initialState, render, update } {}

buildFromForm :: Form -> Select
buildFromForm { name, options, label } = Select name label options

initialForm :: Form
initialForm =
  { label: ""
  , name: ""
  , optionLabel: ""
  , optionValue: ""
  , options: []
  }

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
            [ H.span_ [ H.text "select name" ]
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
            [ H.span_ [ H.text "select label" ]
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
            [ H.span_ [ H.text "option label" ]
            , H.input
              { onChange:
                  capture
                    self
                    targetValue
                    (\v -> EditOptionLabel (fromMaybe "" v))
              , value: form.optionLabel
              }
            ]
          , H.br {}
          , H.label_
            [ H.span_ [ H.text "option value" ]
            , H.input
              { onChange:
                  capture
                    self
                    targetValue
                    (\v -> EditOptionValue (fromMaybe "" v))
              , value: form.optionValue
              }
            ]
          , H.button
            { onClick:
                capture
                  self
                  preventDefault
                  (const AddOption)
            , children: [ H.text "Add" ]
            }
          ]
        , H.div_
          [ H.span_
            [ H.text "options"
            ]
          , H.ul_
            (mapFlipped
              form.options
              (\(Option l v) ->
                H.li_
                [ H.span_
                  [ H.text l ]
                , H.span_
                  [ H.text v ]
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
              Just (Select name label options') ->
                H.label_
                [ H.span_ [ H.text label ]
                , H.select
                  { name
                  , children:
                      mapFlipped
                        options'
                        (\(Option l v) ->
                          H.option { children: [H.text l], value: v })
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
update self@{ state } AddOption =
  Update
    state
      { form =
          state.form
            { optionLabel = ""
            , optionValue = ""
            , options = Array.snoc state.form.options (Option state.form.optionLabel state.form.optionValue)
            }
      }
update self@{ state } BuildForm =
  Update state { built = Just (buildFromForm state.form), form = initialForm }
update self@{ state } (EditLabel v) =
  Update state { form = state.form { label = v } }
update self@{ state } (EditName v) =
  Update state { form = state.form { name = v } }
update self@{ state } (EditOptionLabel v) =
  Update state { form = state.form { optionLabel = v } }
update self@{ state } (EditOptionValue v) =
  Update state { form = state.form { optionValue = v } }
