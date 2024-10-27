module App.PlayerSheet
  ( Action(..)
  , State
  , Output(..)
  , component
  , handleAction
  )
  where

import Prelude

import App.ScoreSheet (Name, ScoreSheet, SheetElement(..), Value(..), ValueMap, insertValue, lookupIntValue, lookupToggleValue, qualifiedName)
import Data.Array ((..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onChecked, onValueInput)
import Halogen.HTML.Properties (InputType(..), checked, classes, type_, value)

type Input = State

data Output 
  = ValueUpdate ValueMap 

type State
  = { valueMap :: ValueMap, sheet :: ScoreSheet }

data Action
  = UpdateField Name Int
  | UpdateToggle Name Boolean
  | Recive State

component :: forall q m. H.Component q Input Output m
component =
  H.mkComponent
    { initialState: identity
    , render: render
    , eval: H.mkEval H.defaultEval 
      { handleAction = handleAction
      , receive = Just <<< Recive
      }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render {valueMap, sheet} =
  renderSheet valueMap sheet

handleAction :: forall cs m. Action → H.HalogenM State Action cs Output m Unit
handleAction (UpdateField name val) = do
  st <- H.modify (\st -> st{valueMap = insertValue name (FieldValue val) st.valueMap})
  H.raise $ ValueUpdate st.valueMap
handleAction (UpdateToggle name val) = do
  st <- H.modify (\st -> st{valueMap = insertValue name (ToggleValue val) st.valueMap})
  H.raise $ ValueUpdate st.valueMap
handleAction (Recive st) = H.put st

renderSheet :: forall w. ValueMap -> ScoreSheet -> HH.HTML w Action
renderSheet env xs = HH.div [classes [ClassName ""]] $ 
  (map (renderEntry identity env) xs)

renderEntry :: forall w. (Name -> Name) -> ValueMap -> SheetElement -> HH.HTML w Action
renderEntry qualify env (SheetRound name n expr vs xs) = 
  HH.div_ $
    map (\i -> renderRound qualify i env xs) (1 .. n)
    <> [  HH.div_ [ HH.text $ name <> ": " <> maybe "" show (lookupIntValue (qualify name) env) ] ]
renderEntry qualify env (SheetField name) = 
  HH.div_
    [ HH.text $ name <> ": "
    , HH.input 
      [ value $ maybe "" show (lookupIntValue (qualify name) env)
      , type_ InputNumber
      , onValueInput $ \x -> UpdateField (qualify name) (fromMaybe 0 $ fromString x)
      ]
    ]
renderEntry qualify env (SheetToggle name) = 
  HH.div_
    [ HH.text $ name <> ": "
    , HH.input 
      [ checked $ fromMaybe false (lookupToggleValue (qualify name) env)
      , type_ InputCheckbox
      , onChecked $ \x -> UpdateToggle (qualify name) x
      ]
    ]
renderEntry qualify env (SheetResult name xs) = 
  HH.div_ 
    [ HH.text $ name <> ": " <> show (fromMaybe 0 $ lookupIntValue (qualify name) env)
    ]


renderRound :: forall w. (Name -> Name) -> Int -> ValueMap -> Array SheetElement -> HH.HTML w Action
renderRound qualify n env xs = 
  HH.div [classes [ClassName "w-fit border"]] $
    [ HH.span [classes [ClassName "font-semibold"]] [HH.text $ "Round " <> show n]
    ] <> map (renderEntry (qualify >>> qualifiedName n) env) xs

horizontalLine ∷ forall w i. HH.HTML w i
horizontalLine = HH.hr [classes [ClassName "h-px bg-black border-0"]]

nbsp ∷ String
nbsp = "\xa0"
