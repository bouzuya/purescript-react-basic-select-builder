module Component.SelectsForm
  ( selectsForm
  ) where


import Component.OptionsForm as OptionsForm
import Data.Functor (mapFlipped)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Prelude (Unit, const, (>>>))
import React.Basic (Component, JSX, createComponent, makeStateless)
import React.Basic.DOM as H
import React.Basic.DOM.Events (preventDefault, stopPropagation, targetValue)
import React.Basic.Events (handler)

type Props =
  { label :: String
  , name :: String
  , onAddOption :: Effect Unit
  , onAddSelect :: Effect Unit
  , onChangeLabel :: String -> Effect Unit
  , onChangeName :: String -> Effect Unit
  , onChangeOptionLabel :: String -> Effect Unit
  , onChangeOptionValue :: String -> Effect Unit
  , optionLabel :: String
  , optionValue :: String
  , options :: Array { label :: String, value :: String }
  , selects :: Array { label :: String, name :: String, options :: Array { label :: String, value :: String } }
  }

component :: Component Props
component = createComponent "App"

-- | selects form component
selectsForm :: Props -> JSX
selectsForm = makeStateless component render

render :: Props -> JSX
render props =
  H.div_
  [ H.label_
    [ H.span_ [ H.text "select name" ]
    , H.input
      { onChange:
          handler
            (preventDefault >>> stopPropagation >>> targetValue)
            (\v -> props.onChangeName (fromMaybe "" v))
      , value: props.name
      }
    ]
  , H.br {}
  , H.label_
    [ H.span_ [ H.text "select label" ]
    , H.input
      { onChange:
          handler
            (preventDefault >>> stopPropagation >>> targetValue)
            (\v -> props.onChangeLabel (fromMaybe "" v))
      , value: props.label
      }
    ]
  , H.br {}
  , OptionsForm.optionsForm
    { label: props.optionLabel
    , onAddOption: props.onAddOption
    , onChangeLabel: props.onChangeOptionLabel
    , onChangeValue: props.onChangeOptionValue
    , options: props.options
    , value: props.optionValue
    }
  , H.div_
    [ H.button
      { onClick:
          handler
            (preventDefault >>> stopPropagation)
            (const props.onAddSelect)
      , children: [ H.text "Add Select" ]
      }
    ]
  , H.div_
    [ H.span_
      [ H.text "selects"
      ]
    , H.ul_
      (mapFlipped
        props.selects
        (\{ label, name, options } ->
          H.div_
          [ H.span_ [ H.text name ]
          , H.span_ [ H.text label ]
          , H.ul_
              (mapFlipped
                options
                (\{ label: optionLabel, value: optionValue } ->
                  H.li_
                  [ H.span_
                    [ H.text optionLabel ]
                  , H.span_
                    [ H.text ":" ]
                  , H.span_
                    [ H.text optionValue ]
                  ]))
          ]
        )
      )
    ]
  ]
