module Lexer (Token(..), PosToken, tokens) where

import Text.Parsec            (SourcePos, getPosition, try, many1, many, count, noneOf, char, string, digit, hexDigit, eof, newline, optional, skipMany, (<|>))
import Text.Parsec.String     (Parser)
import Text.Parsec.Char       (space) -- Importera space
import Text.Parsec.Combinator (choice, lookAhead) -- Importera lookAhead

-- | Token-typer (med REP Int, OPEN_QUOTE, CLOSE_QUOTE)
data Token
  = COLOR | HEX String | FORW | BACK | LEFT | RIGHT | UP | DOWN
  | REP_KW -- Nyckelordet REP
  | DECIMAL Int | OPEN_QUOTE | CLOSE_QUOTE | PERIOD | ERROR
  deriving (Show, Eq)

type PosToken = (Token, SourcePos)
type LexerParser a = Parser a

-- | Skippa whitespace och kommentarer
sc :: Parser ()
sc = skipMany (space <|> comment)

comment :: Parser Char
comment = do
  _ <- char '%'
  _ <- skipMany (noneOf "\n")
  _ <- optional newline
  return ' '  -- Dummy

-- | Läser strängen s, kräver space efter (med lookAhead), konsumerar sc.
symbol :: String -> Parser String
symbol s = try (string s <* sc) -- Borttaget: lookAhead space

-- | Konsumerar sc före och efter parsern p.
lexeme :: Parser a -> Parser a
lexeme p = try (sc *> p <* sc)

-- | Hjälpfunktion för PosToken
posTok :: LexerParser Token -> LexerParser PosToken
posTok p = do
  pos <- getPosition
  tok <- p
  return (tok, pos)

-- | Token-parsers enligt den nya strategin

-- COLOR kräver space efter
tokColor :: LexerParser PosToken
tokColor = posTok (COLOR <$ try (string "COLOR" <* sc)) -- Borttaget: lookAhead space

-- HEX använder lexeme runt # och siffrorna
tokHex :: LexerParser PosToken
tokHex = posTok (HEX <$> lexeme (char '#' *> count 6 hexDigit))

-- Kommandon använder symbol (kräver space efter)
tokForw :: LexerParser PosToken
tokForw = posTok (FORW <$ symbol "FORW")

tokBack :: LexerParser PosToken
tokBack = posTok (BACK <$ symbol "BACK")

tokLeft :: LexerParser PosToken
tokLeft = posTok (LEFT <$ symbol "LEFT")

tokRight :: LexerParser PosToken
tokRight = posTok (RIGHT <$ symbol "RIGHT")

tokUp :: LexerParser PosToken
tokUp = posTok (UP <$ symbol "UP")

tokDown :: LexerParser PosToken
tokDown = posTok (DOWN <$ symbol "DOWN")

-- REP använder symbol (kräver space efter)
-- Vi behåller REP Int här, men matchar bara "REP" + space. Siffran blir en separat DECIMAL token.
tokRepKw :: LexerParser PosToken
tokRepKw = posTok (REP_KW <$ symbol "REP") -- Ny token REP_KW bara för nyckelordet

-- DECIMAL använder lexeme (konsumerar sc före och efter siffrorna)
tokDecimal :: LexerParser PosToken
tokDecimal = posTok (DECIMAL . read <$> lexeme (many1 digit))

-- OPEN_QUOTE använder lexeme (förlitar sig på att föregående DECIMAL konsumerade sc)
tokOpenQuote :: LexerParser PosToken
tokOpenQuote = posTok (OPEN_QUOTE <$ lexeme (char '"'))

-- CLOSE_QUOTE använder lexeme
tokCloseQuote :: LexerParser PosToken
tokCloseQuote = posTok (CLOSE_QUOTE <$ lexeme (char '"'))

-- PERIOD använder lexeme
tokPeriod :: LexerParser PosToken
tokPeriod = posTok (PERIOD <$ lexeme (char '.'))

whitespaceChars :: String
whitespaceChars = " \t\n\r%" -- kanske ta bort %

-- Felhantering (om nödvändigt, men vi förlitar oss på parsern)
tokError = posTok (ERROR <$ lexeme (many1 (noneOf whitespaceChars))) -- Använd lexeme här också

-- | Lista med alla token-parsers i rätt ordning
singleToken :: LexerParser PosToken
singleToken = choice $ map try
  [ tokColor, tokForw, tokBack, tokLeft, tokRight, tokUp, tokDown, tokRepKw -- Nyckelord först
  , tokHex
  , tokDecimal -- Efter nyckelord
  , tokOpenQuote, tokCloseQuote
  , tokPeriod
  , tokError
  ]

-- | Hela token-strömmen
tokens :: LexerParser [PosToken]
tokens = sc *> many singleToken <* eof
