module App where

import Prelude

import Control.MonadPlus (guard)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select as Select
import Select.Setters as Setters

type Message
  = Void

data Query a
  = Init a
  | HandleDropdown (Select.Message Query Unit) a

type State =
  { value :: String
  }

type Slot =
  ( select :: Select.Slot Query () Unit Aff Unit
  )

_select = SProxy :: SProxy "select"

type SelectHTML = Select.HTML Query () Unit Aff

type HTML = H.ComponentHTML Query Slot Aff

type DSL = H.HalogenM State Query Slot Message Aff

initialState :: State
initialState =
  { value: ""
  }

class_ :: forall r i. String -> HP.IProp ("class" :: String | r) i
class_ = HP.class_ <<< HH.ClassName

renderContainer
  :: Select.State Unit
  -> SelectHTML
renderContainer st =
  HH.div_ $ join
  [ pure $ HH.button
    ( Setters.setToggleProps [] )
    [ HH.text "toggle" ]
  , guard (st.visibility == Select.On) $> HH.div
    [ class_ "absolute pin-r pt-3 z-dropdown" ]
    [ HH.div
      (Setters.setContainerProps
      [ class_ "bg-white shadow-md overflow-hidden" ]
      ) $
      [ HH.input
        [ HP.type_ HP.InputText ]
      ]
    ]
  ]

render :: State -> HTML
render state =
  HH.div [ class_ "relative cursor-pointer" ]
  [ HH.slot _select unit Select.component
    { initialSearch: Nothing
    , debounceTime: Nothing
    , inputType: Select.Toggle
    , items: []
    , render: renderContainer
    } $
    HE.input HandleDropdown
  ]

app :: H.Component HH.HTML Query Unit Message Aff
app = H.component
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  , initializer: Nothing
  , finalizer: Nothing
  }
  where
  eval :: Query ~> DSL
  eval (Init n) = n <$ do
    pure unit

  eval (HandleDropdown _ n) = n <$ do
    pure unit
