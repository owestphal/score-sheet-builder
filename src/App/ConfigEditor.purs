module App.ConfigEditor where

import Prelude

import App.Parser (parseScoreSheet)
import App.ScoreSheet (ScoreSheet, printSheetConfig)
import Data.Array (intersperse)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Base64 as Base64
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick, onValueInput)
import Halogen.HTML.Properties (classes, cols, rows, value)
import Parsing (ParseError)
import Web.HTML (window)
import Web.HTML.Location (setSearch)
import Web.HTML.Window (location)

type Input = ScoreSheet

data Output
  = NewConfig ScoreSheet

type State = 
  { editorContent :: String
  , lastError :: Maybe ParseError
  }

data Action
  = UpdateEditor String
  | SaveToURL

component :: forall q m. MonadEffect m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState: \sheet -> 
      { editorContent: printSheetConfig sheet
      , lastError: Nothing
      }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

handleAction :: forall cs m. MonadEffect m => Action → H.HalogenM State Action cs Output m Unit
handleAction (UpdateEditor str) = do
  case parseScoreSheet str of
    Left err -> H.modify_ _{ editorContent = str, lastError = Just err }
    Right sheet -> do 
      H.modify_ $ _{ editorContent = str, lastError = Nothing }
      H.raise $ NewConfig sheet
handleAction SaveToURL = do
  st <- H.get
  H.liftEffect $ do
    let encoded = Base64.encode st.editorContent
    window >>= location >>= setSearch encoded

render :: forall m s. State -> H.ComponentHTML Action s m
render {editorContent, lastError} = HH.div [classes [ClassName "flex"]]
  [ HH.div [ classes [ClassName "font-mono m-4"] ]
    [ HH.textarea
      [ classes [ClassName "border"]
      , value editorContent
      , cols 100
      , rows 10
      , onValueInput UpdateEditor
      ]
    , HH.br_        
    , HH.div [classes [ClassName "my-4"]]
      [ HH.text $ "Status: " <> show lastError
      , HH.button [classes [ClassName "m-4 border"], onClick $ const SaveToURL ] [HH.text "Save"]
      ]
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