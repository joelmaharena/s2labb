-- app/Parser.hs
{-# LANGUAGE LambdaCase #-}
module Parser (AST(..), Direction(..), parseProgram) where

import Text.Parsec            (Parsec, try, many, eof, (<|>))
import Text.Parsec.Prim       (tokenPrim)
import Lexer                  (Token(..))

-- | Parser över listor av dina Token
type Parser a = Parsec [Token] () a

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
matchToken test = tokenPrim show updatePos matchFn
  where
    matchFn t         = test t
    updatePos pos _ _ = pos

-- | Flytta-kommandon
parseMove :: Parser AST
parseMove = do
  dir <- matchToken $ \case
    FORW -> Just DirForw
    BACK -> Just DirBack
    _    -> Nothing
  n <- matchToken $ \case
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

-- | Loop med citattecken: REP n " ... " .
parseLoopQuoted :: Parser AST
parseLoopQuoted = do
  _    <- matchToken (\t -> if t == REP then Just () else Nothing)
  n    <- matchToken $ \case DECIMAL x -> Just x; _ -> Nothing
  _    <- matchToken (\t -> if t == QUOTE then Just () else Nothing)
  body <- many parseCommand
  _    <- matchToken (\t -> if t == QUOTE then Just () else Nothing)
  return (Loop n body)

-- | Loop utan citattecken (endast en enda kommando i kroppen)
parseLoopUnquoted :: Parser AST
parseLoopUnquoted = do
  _   <- matchToken (\t -> if t == REP then Just () else Nothing)
  n   <- matchToken $ \case DECIMAL x -> Just x; _ -> Nothing
  cmd <- parseCommand  -- kommer konsumera sin avslutande PERIOD
  return (Loop n [cmd])

parseLoop :: Parser AST
parseLoop = try parseLoopQuoted <|> parseLoopUnquoted

-- | Alla möjliga kommandon
parseCommand :: Parser AST
parseCommand =  try parseMove
            <|> try parseTurn
            <|> try parsePen
            <|> try parseColor
            <|> parseLoop

-- | Hela programmet
parseProgram :: Parser [AST]
parseProgram = many parseCommand <* eof
