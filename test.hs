{-# LANGUAGE OverloadedStrings #-}
import Text.Megaparsec        (Parsec, (<|>), many, eof, parseTest, count, empty)
import Text.Megaparsec.Char   (space1, char, letterChar, hexDigitChar, digitChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void              (Void)

type Parser = Parsec Void String

sc :: Parser ()              -- *skip* ‑ parser för mellanslag & kommentarer
sc = L.space space1 lineCmnt empty 
  where lineCmnt  = L.skipLineComment "%"


lexeme  = L.lexeme sc
symbol  = L.symbol sc

data Token 
  = COLOR                     -- "COLOR"
  | HEX                       -- '#' följt av 6 hex-siffror
  | FORW
  | BACK
  | LEFT
  | RIGHT 
  | UP                        -- "UP"
  | DOWN 
  | REP 
  | DECIMAL Int
  | QUOTE
  | PERIOD                    -- "."
  | ERROR
  deriving (Show, Eq)

-- Token Parsers
tokColor :: Parser Token
tokColor = COLOR <$ symbol "COLOR"

tokHex :: Parser Token
tokHex = HEX <$ (symbol "#" *> count 6 hexDigitChar) -- Assuming hexDigitChar is imported or defined

tokForw :: Parser Token
tokForw = FORW <$ symbol "FORW"

tokBack :: Parser Token
tokBack = BACK <$ symbol "BACK"

tokLeft :: Parser Token
tokLeft = LEFT <$ symbol "LEFT"

tokRight :: Parser Token
tokRight = RIGHT <$ symbol "RIGHT"

tokUp :: Parser Token
tokUp = UP <$ symbol "UP"

tokDown :: Parser Token
tokDown = DOWN <$ symbol "DOWN"

tokRep :: Parser Token
tokRep = REP <$ symbol "REP"

tokDecimal :: Parser Token
tokDecimal = DECIMAL <$> lexeme L.decimal -- Use the Int from the data type

tokQuote :: Parser Token
tokQuote = QUOTE <$ symbol "\""

tokPeriod :: Parser Token
tokPeriod = PERIOD <$ symbol "."

-- Combined Token Parser
token :: Parser Token
token = tokColor <|> tokHex <|> tokForw <|> tokBack <|> tokLeft <|> tokRight <|>
        tokUp <|> tokDown <|> tokRep <|> tokDecimal <|> tokQuote <|> tokPeriod

tokens :: Parser [Token]
tokens = many token <* eof

-- Update main to test the new tokens
main :: IO ()
main = parseTest tokens "DOWN.\
\FORW 1. LEFT 90.\
\FORW 1. LEFT 90.\
\FORW 1. LEFT 90.\
\FORW 1. LEFT 90."