module App.MainUI where

import App.ScoreSheet
import Prelude

import App.Parser (parseScoreSheet)
import App.PlayerSheet as PlayerSheet
import Data.Array (intersperse)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onValueInput)
import Halogen.HTML.Properties (classes, cols, rows, value)
import Parsing (ParseError(..))
import Type.Proxy (Proxy(..))

type State = { editorContent :: String, sheetConfig :: ScoreSheet, lastError :: Maybe ParseError }

data Action =
  UpdateEditor String

type Slots = ( playerSheet :: forall query. H.Slot query Void Int )

_playerSheet = Proxy :: Proxy "playerSheet"

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> 
      { editorContent: printSheetConfig exampleSheet
      , sheetConfig: exampleSheet
      , lastError: Nothing
      }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
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

handleAction :: forall cs o m. Action → H.HalogenM State Action cs o m Unit
handleAction (UpdateEditor str) = do
  case parseScoreSheet str of
    Left err -> H.modify_ $ _{ editorContent = str, lastError = Just err }
    Right sheet -> H.modify_ $ _{ editorContent = str, sheetConfig = sheet, lastError = Nothing }

render :: forall m. State -> H.ComponentHTML Action Slots m
render {editorContent, sheetConfig, lastError} = HH.div [classes [ClassName "flex"]]
  [ HH.slot_ _playerSheet 1 PlayerSheet.component sheetConfig
  , HH.div [ classes [ClassName "font-mono m-4"] ]
    [ HH.textarea
      [ classes [ClassName "border"]
      , value editorContent
      , cols 100
      , rows 10
      , onValueInput UpdateEditor
      ]
    , HH.br_        
    , HH.div [classes [ClassName "my-4"]]
      [ HH.text $ "Status: " <> show lastError ]
    , HH.div [classes [ClassName ""]]$ 
      unlinesHTML
        [ "element:"
        , "field <name>"
        , "check <name>"
        , "result <name> = <expr>"
        , "round <name> <number> sum { <element>, ... }"
        , ""
        , "expr:"
        , "<number>"
        , "field <name>"
        , "threshold <name> [<threshold>: <number>, ...]"
        , "[<name>] {<expr>} {<expr>} (if-then-else)"
        ]
    ]
  ]

unlinesHTML :: forall w i. Array String -> Array (HH.HTML w i)
unlinesHTML =  map HH.text >>> intersperse HH.br_