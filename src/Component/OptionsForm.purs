module Component.OptionsForm
  ( optionsForm
  ) where

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
  , onAddOption :: Effect Unit
  , onChangeLabel :: String -> Effect Unit
  , onChangeValue :: String -> Effect Unit
  , options :: Array { label :: String, value :: String }
  , value :: String
  }

component :: Component Props
component = createComponent "OptionsForm"

-- | options form component
optionsForm :: Props -> JSX
optionsForm = makeStateless component render

render :: Props -> JSX
render props =
  H.div_
  [ H.label_
    [ H.span_ [ H.text "option label" ]
    , H.input
      { onChange:
          handler
            (preventDefault >>> stopPropagation >>> targetValue)
            (\v -> props.onChangeLabel (fromMaybe "" v))
      , value: props.label
      }
    ]
  , H.br {}
  , H.label_
    [ H.span_ [ H.text "option value" ]
    , H.input
      { onChange:
          handler
            (preventDefault >>> stopPropagation >>> targetValue)
            (\v -> props.onChangeValue (fromMaybe "" v))
      , value: props.value
      }
    ]
  , H.button
    { onClick:
        handler
          (preventDefault >>> stopPropagation)
          (const props.onAddOption)
    , children: [ H.text "Add Option" ]
    }
  , H.div_
    [ H.span_
      [ H.text "options"
      ]
    , H.ul_
      (mapFlipped
        props.options
        (\{ label, value } ->
          H.li_
          [ H.span_
            [ H.text label ]
          , H.span_
            [ H.text ":" ]
          , H.span_
            [ H.text value ]
          ]))
    ]
  ]
