module App.Parser where

import App.ScoreSheet (AggregateFunction(..), Name, ScoreSheet, SheetElement(..), ValueExpr(..), sumExpr)
import Control.Lazy (defer)
import Data.Array (fromFoldable, many)
import Data.Array.NonEmpty (toArray)
import Data.Either (Either)
import Data.Map as Map
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Parsing (ParseError, Parser, runParser)
import Parsing.Combinators (between, chainl, chainl1, notFollowedBy, sepBy, try, (<|>))
import Parsing.Combinators.Array (many1)
import Parsing.Expr (Assoc(..), Operator(..), buildExprParser)
import Parsing.String (char, eof, string)
import Parsing.String.Basic (alphaNum, intDecimal, whiteSpace)
import Prelude (Unit, bind, discard, pure, void, ($), ($>), (*>), (<$>), (<*), (<*>), (<<<), (>>>))

parseScoreSheet :: String -> Either ParseError ScoreSheet
parseScoreSheet str = runParser str (fully scoreSheet)

-- ScoreSheet
scoreSheet :: Parser String ScoreSheet
scoreSheet = many sheetElement

-- SheetElement
sheetElement :: Parser String SheetElement
sheetElement = defer \_ ->
  sheetField <|> sheetToggle <|> sheetResult <|> sheetRound

sheetField :: Parser String SheetElement
sheetField = do
  keyword "field"
  SheetField <$> ident

sheetToggle :: Parser String SheetElement
sheetToggle = do
  keyword "check"
  SheetToggle <$> ident

sheetResult :: Parser String SheetElement
sheetResult = do 
  keyword "result"
  SheetResult <$> ident <*> (lexStr "=" *> valueExpr )

sheetRound :: Parser String SheetElement
sheetRound = do
  keyword "round"
  name <- ident
  n <- lexeme intDecimal
  expr <- aggregateExpr
  vs <-  arrayOf ident
  xs <- between (lexStr "{") (lexStr "}") (toArray <$> many1 sheetElement)
  pure $ SheetRound name n expr vs xs

aggregateExpr :: Parser String AggregateFunction
aggregateExpr = keyword "sum" $> Sum

-- ValueExpr
valueExpr :: Parser String ValueExpr
valueExpr = sumExpr
  where
    sumExpr = chainl1 prodExpr (lexStr "+" $> Add)  
    prodExpr = chainl1 base (lexStr "*" $> Mul)
    base = defer \_ -> constExpr <|> fieldExpr <|> thresholdExpr <|> ifExpr <|> nested
    nested = defer \_ -> between (lexStr "(") (lexStr ")") valueExpr

constExpr :: Parser String ValueExpr
constExpr = Const <$> lexeme intDecimal

fieldExpr :: Parser String ValueExpr
fieldExpr = do
  keyword "field"
  Field <$> ident

thresholdExpr :: Parser String ValueExpr
thresholdExpr = do
  keyword "threshold"
  name <- ident
  xs <- arrayOf pair
  pure $ ThresholdField name (Map.fromFoldable xs)

pair :: Parser String (Tuple Int Int)
pair = Tuple <$> (lexeme intDecimal <* lexStr ":") <*> lexeme intDecimal

ifExpr :: Parser String ValueExpr
ifExpr = defer \_ -> IfThenElse <$> 
  (between (lexStr "[") (lexStr "]") ident)
  <*> between (lexStr "{") (lexStr "}") valueExpr
  <*> between (lexStr "{") (lexStr "}") valueExpr

-- basics
lexeme :: forall a. Parser String a -> Parser String a
lexeme p = p <* whiteSpace 

token :: forall a. Parser String a -> Parser String a
token = try >>> lexeme

keyword :: String -> Parser String Unit
keyword k = token (string k *> notFollowedBy alphaNum)

ident :: Parser String String
ident = fromCharArray <<< toArray <$> (lexeme (many1 alphaNum))

lexStr :: String -> Parser String Unit
lexStr = void <<< lexeme <<< string

arrayOf :: forall a. Parser String a -> Parser String (Array a) 
arrayOf p = fromFoldable <$> (between (lexStr "[") (lexStr "]") $ p `sepBy` (lexStr ","))
 
fully :: forall a. Parser String a -> Parser String a 
fully p = whiteSpace *> p <* eof