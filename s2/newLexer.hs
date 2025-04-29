-- app/Lexer.hs
module Lexer (Token(..), PosToken, tokens) where -- Exportera PosToken

import Text.Parsec            (SourcePos, getPosition, try, many1, many, count, noneOf, char, string, digit, hexDigit, eof, newline, optional, skipMany, (<|>), satisfy, letter)
import Text.Parsec.String     (Parser)
import Text.Parsec.Char       (space)
import Text.Parsec.Combinator (choice, lookAhead)
import Control.Monad (void)
import Data.Char (toLower) -- Importera toLower

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

-- | Case-insensitive version av 'char'
charCI :: Char -> Parser Char
charCI c = satisfy (\x -> toLower x == toLower c)

-- | Case-insensitive version av 'string'
stringCI :: String -> Parser String
stringCI s = try (mapM charCI s) -- Använd try för att kunna backa

-- | Läser exakt strängen s (case-insensitive), och skippar sedan sc
symbolCI :: String -> Parser String
symbolCI s = try (stringCI s <* sc)

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
tokColor = posTok $ try $ do -- Lägg till try här för att kunna backa om lookAhead misslyckas
    _ <- stringCI "COLOR" -- Matcha COLOR case-insensitive
    -- Kräv att nästa tecken är whitespace, EOF, eller början på en kommentar
    lookAhead (eof <|> void space <|> void (char '%'))
    sc -- Konsumera whitespace/kommentar om det fanns
    return COLOR

tokHex :: LexerParser PosToken
-- symbol hanterar inte case-insensitivity, använd lexeme och stringCI
tokHex = posTok (HEX <$> lexeme (char '#' *> count 6 hexDigit))

tokForw :: LexerParser PosToken
tokForw = posTok (FORW <$ symbolCI "FORW") -- Använd symbolCI

tokBack :: LexerParser PosToken
tokBack = posTok (BACK <$ symbolCI "BACK") -- Använd symbolCI

tokLeft :: LexerParser PosToken
tokLeft = posTok (LEFT <$ symbolCI "LEFT") -- Använd symbolCI

tokRight :: LexerParser PosToken
tokRight = posTok (RIGHT <$ symbolCI "RIGHT") -- Använd symbolCI

tokUp :: LexerParser PosToken
tokUp = posTok (UP <$ symbolCI "UP") -- Använd symbolCI

tokDown :: LexerParser PosToken
tokDown = posTok (DOWN <$ symbolCI "DOWN") -- Använd symbolCI

tokRep :: LexerParser PosToken
tokRep = posTok (REP <$ symbolCI "REP") -- Använd symbolCI

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
-- Använd lexeme för att hantera whitespace runt citattecken
tokQuote = posTok (QUOTE <$ lexeme (char '"'))

tokPeriod :: LexerParser PosToken
-- Använd lexeme för att hantera whitespace runt punkt
tokPeriod = posTok (PERIOD <$ lexeme (char '.'))

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