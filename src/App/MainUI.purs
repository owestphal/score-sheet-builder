module App.MainUI where

import Prelude

import App.ConfigEditor (Output(..))
import App.ConfigEditor as ConfigEditor
import App.Parser (parseScoreSheet)
import App.PlayerSheet (Output(..))
import App.PlayerSheet as PlayerSheet
import App.ScoreSheet (AggregateFunction(..), SheetElement(..), ValueExpr(..), ValueMap, ScoreSheet, initialValueMap)
import Data.Array (foldr, intersperse, (..))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (drop)
import Data.String.Base64 as Baser64
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick, onValueInput)
import Halogen.HTML.Properties (InputType(..), classes, type_, value)
import Type.Proxy (Proxy(..))
import Web.HTML (window)
import Web.HTML.Location (search)
import Web.HTML.Window (location)

type State = 
  { sheetConfig :: ScoreSheet
  , scoreMap :: Map Int ValueMap
  , showEditor :: Boolean
  , numberOfPlayers :: Int
  , playerNames :: Map Int String
  }

data Action 
  = ToggleEditor
  | ChangeNumberOfPlayers Int
  | UpdatePlayerName Int String
  | HandlePlayerSheet Int PlayerSheet.Output
  | HandleEditor ConfigEditor.Output
  | Initialize

type Slots = 
  ( playerSheet :: forall query. H.Slot query PlayerSheet.Output Int
  , configEditor :: forall query. H.Slot query ConfigEditor.Output Unit
  )

_playerSheet = Proxy :: Proxy "playerSheet"
_configEditor = Proxy :: Proxy "configEditor"

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> 
      { sheetConfig: exampleSheet
      , showEditor: false
      , numberOfPlayers: 2
      , scoreMap: newScoreMap 2 exampleSheet
      , playerNames: Map.empty
      }
    , render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
    }

exampleSheet :: ScoreSheet
exampleSheet = 
  [ SheetRound "Final" 3 Sum ["Cards", "Bonus"]
    [ SheetField "Cards"
    , SheetField "Bonus"
    , SheetToggle "kaputt"
    , SheetResult "RoundTotal" $ 
      IfThenElse "kaputt" 
        (Const (-1) `Mul` Field "Cards")
        (Add (Field "Cards") (Mul (Const 2) (ThresholdField "Bonus" $ Map.fromFoldable [1 /\ 1, 2 /\ 2, 3 /\ 5])))
    ]
  ]

newScoreMap :: Int -> ScoreSheet -> Map Int ValueMap
newScoreMap n sheet = Map.fromFoldable $ map (_ /\ initialValueMap sheet) (1..n)

extendScoreMap :: Int -> ScoreSheet -> Map Int ValueMap -> Map Int ValueMap
extendScoreMap n sheet m = 
  foldr (\i -> Map.insertWith const i (initialValueMap sheet) ) m $ (1 .. n)

handleAction :: forall cs o m. MonadEffect m => Action -> H.HalogenM State Action cs o m Unit
handleAction Initialize = do
  sheetE <- H.liftEffect do
    sStr <- window >>= location >>= search
    pure $ do 
      decoded <- lmap show $ Baser64.decode $ drop 1 sStr
      lmap show $ parseScoreSheet decoded
  case sheetE of
    Left _ -> pure unit
    Right [] -> pure unit
    Right sheet -> handleAction $ HandleEditor $ NewConfig sheet
handleAction ToggleEditor = H.modify_ $ \st -> st{ showEditor = not st.showEditor }
handleAction (ChangeNumberOfPlayers n) = 
  H.modify_ $ \st -> st{ numberOfPlayers = n, scoreMap = extendScoreMap n st.sheetConfig st.scoreMap }
handleAction (UpdatePlayerName i name) =
  H.modify_ $ \st -> st{ playerNames = Map.insert i name st.playerNames}
handleAction (HandlePlayerSheet i (ValueUpdate m)) =
  H.modify_ $ \st -> st{ scoreMap = Map.insert i m st.scoreMap }
handleAction (HandleEditor (NewConfig sheet)) =
  H.modify_ $ \st -> st{ sheetConfig = sheet, scoreMap = newScoreMap st.numberOfPlayers sheet }

eitherToMaybe :: forall a b. Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing 
eitherToMaybe (Right x) = Just x

render :: forall m. MonadEffect m => State ->  H.ComponentHTML Action Slots m
render st@{ sheetConfig, showEditor, numberOfPlayers } = 
  HH.div_ 
  [ renderPlayerSheets st
  , HH.div
    [classes [ClassName "my-10"]]
    [ HH.span [classes [ClassName "mr-5"]] 
      [ HH.text "Number of Players"]
    , HH.input
      [ value $ show numberOfPlayers
      , type_ InputNumber
      , onValueInput $ \x -> ChangeNumberOfPlayers (fromMaybe 0 $ fromString x) 
      ]
    ]
  , HH.div 
    [ classes [ClassName "my-20"]]
    $ [ HH.button 
      [ classes [ClassName "border"]
      , onClick $ \_ -> ToggleEditor
      ]
      [ HH.text "Toggle Editor" ]
    ] <> 
    if showEditor then [HH.slot _configEditor unit ConfigEditor.component sheetConfig HandleEditor] else []
  ]

renderPlayerSheets :: forall m. State -> H.ComponentHTML Action Slots m
renderPlayerSheets {numberOfPlayers, sheetConfig, scoreMap, playerNames} = HH.div [classes [ClassName "flex"]] $
  (\i -> maybe (HH.div_ []) 
    (\valueMap -> HH.div [classes [ClassName ""]]
      [ HH.input 
        [ classes [ClassName "border"]
        , value $ fromMaybe "" $ Map.lookup i playerNames
        , type_ InputText
        , onValueInput $ UpdatePlayerName i
        ] 
      , HH.slot _playerSheet i PlayerSheet.component {sheet:sheetConfig,valueMap} (HandlePlayerSheet i)
      ])
    $ Map.lookup i scoreMap 
  ) <$> (1 .. numberOfPlayers)

unlinesHTML :: forall w i. Array String -> Array (HH.HTML w i)
unlinesHTML =  map HH.text >>> intersperse HH.br_