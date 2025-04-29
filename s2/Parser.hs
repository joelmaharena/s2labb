-- app/Parser.hs
{-# LANGUAGE LambdaCase #-}
module Parser (AST(..), Direction(..), parseProgram) where

import Text.Parsec            (getInput, lookAhead, optionMaybe, Parsec, SourcePos, try, many, eof, (<|>))
import Text.Parsec.Prim       (tokenPrim)
import Lexer                  (Token(..), PosToken)

-- | Parser över listor av dina Token
type Parser a = Parsec [PosToken] () a

-- | AST för språket
data AST
  = Move        Direction Int
  | TurnLeft    Int
  | TurnRight   Int
  | ChangeColor String
  | Loop        Int [AST]
  | PenUp
  | PenDown
  deriving (Show, Eq)

data Direction = DirForw | DirBack
  deriving (Show, Eq)

-- | Hjälp för att ta ett Token om det matchar test
matchToken :: (Token -> Maybe x) -> Parser x
matchToken test = tokenPrim showTok updatePos matchFn
  where
    -- Visa bara token-delen i felmeddelanden
    showTok :: PosToken -> String
    showTok (tok, _) = show tok

    -- Uppdatera positionen till positionen för den konsumerade token
    updatePos :: SourcePos   -- Parsec’s gamla position (kasta bort)
              -> PosToken    -- den token du just konsumerat
              -> [PosToken]  -- resten av tokens
              -> SourcePos
    updatePos _ (_, thisPos) []            = thisPos
    updatePos _ _              ((_, nextPos):_) = nextPos

    -- Kör testfunktionen på token-delen av PosToken
    matchFn (tok, _) = test tok

-- | Flytta-kommandon
parseMove :: Parser AST
parseMove = do
  dir <- matchToken $ \case
    FORW -> Just DirForw
    BACK -> Just DirBack
    _    -> Nothing
  n <- matchToken $ \case
    DECIMAL 0 -> Nothing 
    DECIMAL x -> Just x
    _         -> Nothing
  _ <- matchToken (\t -> if t == PERIOD then Just () else Nothing)
  return (Move dir n)

-- | Vrida-kommandon
parseTurn :: Parser AST
parseTurn = do
  dirMod <- matchToken $ \case
    LEFT  -> Just 1
    RIGHT -> Just (-1)
    _     -> Nothing
  n <- matchToken $ \case
    DECIMAL 0 -> Nothing 
    DECIMAL x -> Just x
    _         -> Nothing
  _ <- matchToken (\t -> if t == PERIOD then Just () else Nothing)
  return $ if dirMod == 1 then TurnLeft n else TurnRight n

-- | PenUp / PenDown
parsePen :: Parser AST
parsePen = do
  p <- matchToken $ \case
    UP   -> Just PenUp
    DOWN -> Just PenDown
    _    -> Nothing
  _ <- matchToken (\t -> if t == PERIOD then Just () else Nothing)
  return p

-- | ChangeColor
parseColor :: Parser AST
parseColor = do
  _   <- matchToken (\t -> if t == COLOR then Just () else Nothing)
  hex <- matchToken $ \case
    HEX code -> Just code
    _        -> Nothing
  _   <- matchToken (\t -> if t == PERIOD then Just () else Nothing)
  return (ChangeColor hex)

parseLoop :: Parser AST
parseLoop = do
  -- REP n …
  _ <- matchToken (\t -> if t == REP then Just () else Nothing)
  n <- matchToken $ \case
    DECIMAL 0 -> Nothing -- Avvisa noll
    DECIMAL x -> Just x  -- Acceptera andra decimaltal
    _         -> Nothing -- Avvisa andra tokens

  -- Titta men konsumera inte
  nextIsQuote <- optionMaybe (lookAhead (matchToken (\t -> if t == QUOTE then Just () else Nothing)))

  case nextIsQuote of
    -- ► Citerad variant
    Just _ -> do
      _    <- matchToken (\t -> if t == QUOTE then Just () else Nothing)
      body <- many parseCommand
      _    <- matchToken (\t -> if t == QUOTE then Just () else Nothing)
      return (Loop n body)

    -- ► O-citerad variant
    Nothing -> do
      cmd <- parseCommand        -- BACK 1. etc.
      return (Loop n [cmd])

-- | Alla möjliga kommandon
parseCommand :: Parser AST
parseCommand =  try parseMove      -- Lägg till try här
            <|> try parseTurn      -- Och här
            <|> try parsePen       -- Och här
            <|> try parseColor     -- Och här
            <|> parseLoop

-- | Hela programmet
parseProgram :: Parser [AST]
parseProgram = many parseCommand <* eof