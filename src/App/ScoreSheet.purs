module App.ScoreSheet
  ( AggregateFunction(..)
  , Name
  , ScoreSheet(..)
  , SheetElement(..)
  , Threshold
  , Value(..)
  , ValueExpr(..)
  , ValueMap(..)
  , emptyValueMap
  , initialValueMap
  , insertFields
  , insertValue
  , lookupIntValue
  , lookupToggleValue
  , printSheetConfig
  , qualifiedName
  , sumExpr
  , valueMapFromArray
  )
  where

import Prelude

import Control.Lazy (defer)
import Data.Array (concatMap, foldr, intersperse, (..))
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..), uncurry)

type Name = String

type ScoreSheet = Array SheetElement

data SheetElement
  = SheetField Name
  | SheetToggle Name
  | SheetResult Name ValueExpr
  | SheetRound Name Int AggregateFunction (Array Name) (Array SheetElement)

instance showSheetElement :: Show SheetElement where
  show (SheetField name) = "SheetField " <> name
  show (SheetToggle name) = "SheetToogle " <> name
  show (SheetResult name expr) = "SheetResult " <> name <> " $ " <> show expr
  show (SheetRound name n _ vs xs) = "SheetRound " <> name <> " " <> show n <> " " <> show vs <> " " <> show xs

data AggregateFunction = Sum

evalAggregateFunction :: AggregateFunction -> Array Name -> ValueExpr
evalAggregateFunction Sum = sumExpr

type Round = Int

qualifiedName :: Round -> Name -> Name
qualifiedName i = (_ <> ("@" <> show i)) 

type ValueMap = { fields :: Map Name Int, toggles :: Map Name Boolean, derived :: Map Name ValueExpr } 

data Value
  = FieldValue Int
  | ToggleValue Boolean
  | ExprValue ValueExpr

insertValue :: Name -> Value -> ValueMap -> ValueMap
insertValue name (FieldValue x) env@{ fields } = env{ fields = Map.insert name x fields }
insertValue name (ToggleValue x) env@{ toggles } = env{ toggles = Map.insert name x toggles }
insertValue name (ExprValue x) env@{ derived } = env{ derived = Map.insert name x derived }

insertFields :: ValueMap -> Array (Tuple Name Int) -> ValueMap
insertFields = foldr (\(Tuple name x) -> insertValue name (FieldValue x))

lookupIntValue :: Name -> ValueMap -> Maybe Int
lookupIntValue x env@{ fields, derived } = case Map.lookup x fields of
  Just v -> Just v
  Nothing -> evalExpr env <$> Map.lookup x derived

lookupToggleValue :: Name -> ValueMap -> Maybe Boolean
lookupToggleValue x {toggles} = Map.lookup x toggles 

valueMapFromArray :: Array (Tuple Name Value) -> ValueMap
valueMapFromArray = foldr (uncurry insertValue) emptyValueMap

emptyValueMap :: ValueMap
emptyValueMap = { fields: Map.empty, toggles: Map.empty, derived: Map.empty}


initialValueMap :: ScoreSheet -> ValueMap
initialValueMap = foldr (insertElementExpr identity) emptyValueMap
  where
    insertElementExpr :: (Name -> Name) -> SheetElement -> ValueMap -> ValueMap
    insertElementExpr _ (SheetField _) = identity
    insertElementExpr _ (SheetToggle _) = identity
    insertElementExpr qualify (SheetResult name expr) = insertValue (qualify name) (ExprValue (qualifyExpr qualify expr))
    insertElementExpr qualify (SheetRound name n expr ps xs) = insertRoundTotal >>> recursiveInserts
      where
        insertRoundTotal =  insertValue (qualify name) (ExprValue $ evalAggregateFunction expr $ concatMap (\i -> map (qualify >>> qualifiedName i) ps) (1..n))
        recursiveInserts = 
          foldr (>>>) identity $ do
            i <- (1..n)
            x <- xs
            pure $ insertElementExpr (qualify >>> qualifiedName i) x

data ValueExpr -- Int typed
  = Add ValueExpr ValueExpr
  | Mul ValueExpr ValueExpr
  | Const Int
  | Field Name
  | ThresholdField Name (Map Threshold Int)
  | IfThenElse Name ValueExpr ValueExpr

derive instance genericValueExpr :: Generic ValueExpr _

instance showValueExpr :: Show ValueExpr where
  show x = genericShow x

sumExpr :: Array Name -> ValueExpr
sumExpr = foldr (\n e -> Field n `Add` e) (Const 0)

evalExpr :: ValueMap -> ValueExpr -> Int
evalExpr env (Add x y) = evalExpr env x + evalExpr env y 
evalExpr env (Mul x y) = evalExpr env x * evalExpr env y
evalExpr _ (Const x) = x
evalExpr env (Field x) = fromMaybe 0 $ lookupIntValue x env
evalExpr env (ThresholdField x xs) = evalThreshold (fromMaybe 0 $ lookupIntValue x env) xs
evalExpr env (IfThenElse c x y) = 
  case lookupToggleValue c env of
    Just true -> evalExpr env x
    _ -> evalExpr env y

type Threshold = Int

evalThreshold :: Int -> Map Threshold Int -> Int
evalThreshold x m = maybe 0 (_.value) $ Map.lookupLE x m

qualifyExpr :: (Name -> Name) -> ValueExpr -> ValueExpr
qualifyExpr f (Add x y) = Add (qualifyExpr f x) (qualifyExpr f y) 
qualifyExpr f (Mul x y) = Mul (qualifyExpr f x) (qualifyExpr f y) 
qualifyExpr _ (Const x) = Const x
qualifyExpr f (Field x) = Field $ f x
qualifyExpr f (ThresholdField x xs) = ThresholdField (f x) xs
qualifyExpr f (IfThenElse c x y) = IfThenElse (f c) (qualifyExpr f x) (qualifyExpr f y)

printSheetConfig :: ScoreSheet -> String
printSheetConfig = defer \_ -> unlines <<< map printSheetElement 

printSheetElement :: SheetElement -> String
printSheetElement (SheetField name) = "field " <> name 
printSheetElement (SheetToggle name) = "check " <> name
printSheetElement (SheetResult name expr) = "result " <> name <> " = " <> printValueExpr expr
printSheetElement (SheetRound name n Sum vs xs) = 
  "round " <> name <> " " <> show n <> " sum " <> printArray vs <> " {\n" <> printSheetConfig xs <> " }"

printValueExpr :: ValueExpr -> String
printValueExpr (Add x y) = "(" <> printValueExpr x <> ") + (" <> printValueExpr y <> ")"
printValueExpr (Mul x y) = "(" <> printValueExpr x <> ") * (" <> printValueExpr y <> ")"
printValueExpr (Const x) = show x
printValueExpr (Field name) = "field " <> name
printValueExpr (ThresholdField name m) = "threshold " <> name <> " " <> printThresholdMap m
printValueExpr (IfThenElse c t e) = "[" <> c <> "] {" <> printValueExpr t <>  "} {" <> printValueExpr e <> "}" 

printThresholdMap :: Map Threshold Int -> String
printThresholdMap m = 
    printArray
    $ map (\(Tuple x y) -> show x <> ": " <> show y)
    $ Map.toUnfoldable m 

concatStrings ∷ Array String -> String
concatStrings = foldr (<>) ""

unlines ∷ Array String -> String
unlines = concatStrings <<< intersperse "\n"

printArray :: Array String -> String
printArray xs = "[" <> concatStrings (intersperse ", " xs) <> "]"