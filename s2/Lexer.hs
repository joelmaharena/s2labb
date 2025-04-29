-- app/Lexer.hs
module Lexer (Token(..), PosToken, tokens) where -- Exportera PosToken

import Text.Parsec            (SourcePos, getPosition, try, many1, many, count, noneOf, char, string, digit, hexDigit, eof, newline, optional, skipMany, (<|>))
import Text.Parsec.String     (Parser)
import Text.Parsec.Char       (space)
import Text.Parsec.Combinator (choice, lookAhead)
import Control.Monad (void)

-- | Token-typer
data Token
  = COLOR | HEX String | FORW | BACK | LEFT | RIGHT | UP | DOWN | REP 
  | DECIMAL Int | QUOTE | PERIOD | ERROR
  deriving (Show, Eq)

-- | Typ för token med dess position
type PosToken = (Token, SourcePos)

-- | Parser över strängar (som tidigare)
type LexerParser a = Parser a -- Behåll gamla namnet internt om du vill


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

-- | Läser något med parsern p, och skippar sedan sc
lexeme :: Parser a -> Parser a
lexeme p = try (p <* sc)

-- | Varje token-parser
-- Hjälpfunktion för att skapa PosToken
posTok :: LexerParser Token -> LexerParser PosToken
posTok p = do
  pos <- getPosition
  tok <- p
  return (tok, pos)

-- Uppdatera alla tok* funktioner:
tokColor :: LexerParser PosToken
tokColor =
  posTok
    (  COLOR
    <$ try (string "COLOR" <* lookAhead space)  -- kräver blanksteg efter "COLOR"
    <* sc
    )

tokHex :: LexerParser PosToken
tokHex = posTok (HEX <$> (symbol "#" *> count 6 hexDigit)) -- Notera ändring till <$>

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

tokRep :: LexerParser PosToken
tokRep = posTok (REP <$ symbol "REP")

-- Använd lexeme för att konsumera sc efter siffrorna
tokDecimal :: LexerParser PosToken
tokDecimal = posTok $ do
  digits <- many1 digit

  -- ▸ kontrollera nästa tecken med lookAhead  (OBS: ingen konsumtion!)
  lookAhead $
       eof                  -- filen slut här? (Typ: Parser ())
    <|> void space          -- blank / tab / newline (Typ: Parser Char -> Parser ())
    <|> void (char '.')     -- punkt (FORW 10.) (Typ: Parser Char -> Parser ())
    <|> void (char '%')
  -- inget annat duger → token-varianten faller och tokError tar över

  sc                        -- ät upp efterföljande whitespace & kommentarer
  return (DECIMAL (read digits))

tokQuote :: LexerParser PosToken
tokQuote = posTok (QUOTE <$ symbol "\"")

tokPeriod :: LexerParser PosToken
tokPeriod = posTok (PERIOD <$ symbol ".")

-- Definiera whitespaceChars
whitespaceChars :: String
whitespaceChars = " \t\n\r"

-- Felhantering (om du har den) behöver också uppdateras
tokError :: LexerParser PosToken
tokError = posTok (ERROR <$ lexeme (many1 (noneOf whitespaceChars))) -- Använd lexeme här också

-- | Lista med alla token-parsers
singleToken :: LexerParser PosToken
singleToken = choice $ map try -- Använd try för att undvika problem med <|> ordning
  [ tokColor, tokHex, tokForw, tokBack, tokLeft, tokRight, tokUp, tokDown
  , tokRep, tokDecimal, tokQuote, tokPeriod
  , tokError -- Avkommentera om du vill ha felhantering
  ]

-- | Slutligen: en lista av PosTokens
tokens :: LexerParser [PosToken] -- Ändra returtyp
tokens = sc *> many singleToken <* eof