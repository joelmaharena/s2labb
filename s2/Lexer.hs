-- app/Lexer.hs
module Lexer (Token(..), tokens) where

import Text.Parsec            (SourcePos, getPosition, try, many1, many, count, noneOf, char, string, digit, hexDigit, eof, newline, optional, skipMany, (<|>))
import Text.Parsec.String     (Parser)
import Text.Parsec.Char       (space)
import Text.Parsec.Combinator (choice)

-- | Token-typer
data Token
  = COLOR
  | HEX String
  | FORW | BACK
  | LEFT | RIGHT
  | UP | DOWN
  | REP
  | DECIMAL Int
  | QUOTE
  | PERIOD
  deriving (Show, Eq, Ord)


type PosToken = (Token, SourcePos)

-- | Skippa mellanslag, radbrytningar och kommentarer (rader som börjar med ‘%’)
sc :: Parser ()
sc = skipMany (space <|> comment)

comment :: Parser Char
comment = do
  _ <- char '%'
  _ <- skipMany (noneOf "\n")
  _ <- optional newline
  return ' '  -- dummy-Char så att sc:s typ stämmer

-- | Läser exakt strängen s, och skippar sedan sc
symbol :: String -> Parser String
symbol s = try (string s <* sc)

-- | Varje token-parser
tokColor   :: Parser Token; tokColor   = COLOR     <$ symbol "COLOR"
tokHex     :: Parser Token
tokHex     = do
  _      <- string "#"
  ds     <- count 6 hexDigit
  sc
  return (HEX ds)
tokForw    :: Parser Token; tokForw    = FORW      <$ symbol "FORW"
tokBack    :: Parser Token; tokBack    = BACK      <$ symbol "BACK"
tokLeft    :: Parser Token; tokLeft    = LEFT      <$ symbol "LEFT"
tokRight   :: Parser Token; tokRight   = RIGHT     <$ symbol "RIGHT"
tokUp      :: Parser Token; tokUp      = UP        <$ symbol "UP"
tokDown    :: Parser Token; tokDown    = DOWN      <$ symbol "DOWN"
tokRep     :: Parser Token; tokRep     = REP       <$ symbol "REP"
tokDecimal :: Parser Token; tokDecimal = DECIMAL . read <$> (many1 digit <* sc)
tokQuote   :: Parser Token; tokQuote   = QUOTE     <$ symbol "\""
tokPeriod  :: Parser Token; tokPeriod  = PERIOD    <$ symbol "."

-- | Lista med alla token-parsers
singleToken :: Parser Token
singleToken = choice
  [ tokColor, tokHex
  , tokForw, tokBack, tokLeft, tokRight
  , tokUp, tokDown, tokRep
  , tokDecimal, tokQuote, tokPeriod
  ]

-- | Slutligen: en lista av tokens
tokens :: Parser [Token]
tokens = sc *> many singleToken <* eof
