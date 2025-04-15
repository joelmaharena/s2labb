{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import Control.Monad (void)
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, count, many, parseTest, runParser, try, (<|>))
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (char, digitChar, hexDigitChar, string)
import qualified Text.Megaparsec.Char.Lexer as L

-- Definiera parser-typen
type Parser = Parsec Void String

-- Token-datatypen
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

-- Konsumerar ett eller flera mellanslag (enligt <WHITE>)
sc :: Parser ()
sc = L.space (void $ some (char ' ')) MP.empty MP.empty

-- Hjälpfunktioner för att hantera "lexemes" och symboler
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- "COLOR"
pTokenColor :: Parser Token
pTokenColor = TokenColor <$ symbol "COLOR"

-- "UP"
pTokenUp :: Parser Token
pTokenUp = TokenUp <$ symbol "UP"

-- "DOWN"
pTokenDown :: Parser Token
pTokenDown = TokenDown <$ symbol "DOWN"

-- "REP"
pTokenRep :: Parser Token
pTokenRep = TokenRep <$ symbol "REP"

-- Rörelse: FORW, BACK, LEFT, RIGHT
pMovement :: Parser Token
pMovement =  try (TokenMovement Forw <$ symbol "FORW")
         <|> try (TokenMovement Back <$ symbol "BACK")
         <|> try (TokenMovement LeftM <$ symbol "LEFT")
         <|> (TokenMovement RightM <$ symbol "RIGHT")

-- PERIOD: "."
pTokenPeriod :: Parser Token
pTokenPeriod = TokenPeriod <$ symbol "."

-- QUOTE: '"'
pTokenQuote :: Parser Token
pTokenQuote = TokenQuote <$ symbol "\""

-- COLORCODE: '#' följt av exakt 6 hex-siffror
pColorCode :: Parser Token
pColorCode = do
  _ <- char '#'  -- Vi använder char här så att vi inte konsumerar mellanslag automatiskt
  hs <- count 6 hexDigitChar
  return $ TokenColorCode ('#' : hs)

-- NUM: En siffra mellan 1 och 9 följt av valfria siffror
pNum :: Parser Token
pNum = do
  first <- MP.satisfy (\c -> c >= '1' && c <= '9')
  rest <- many digitChar
  return $ TokenNum (read (first:rest))

pToken :: Parser Token
pToken =  try pTokenColor
      <|> try pTokenUp
      <|> try pTokenDown
      <|> try pTokenRep
      <|> try pMovement
      <|> try pColorCode
      <|> try pTokenPeriod
      <|> try pTokenQuote
      <|> pNum

lexer :: Parser [Token]
lexer = between sc MP.eof (many pToken)

main :: IO ()
main = do
  let input = "COLOR   #FFA07A . FORW  5 . UP ."
  case runParser lexer "" input of
    Left err    -> putStrLn (MP.errorBundlePretty err)
    Right toks  -> print toks
