-- app/Parser.hs
{-# LANGUAGE LambdaCase #-}
module Parser (AST(..), Direction(..), parseProgram) where

import Text.Parsec            (Parsec, SourcePos, try, many, eof, (<|>), (<?>))
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
  -- Lägg till matchning för FORW/BACK för att definiera dir
  dir <- matchToken $ \case
    FORW -> Just DirForw
    BACK -> Just DirBack
    _    -> Nothing
  n <- matchToken (\case DECIMAL x -> Just x; _ -> Nothing) <?> "decimal after FORW/BACK"
  _ <- matchToken (\t -> if t == PERIOD then Just () else Nothing) <?> "period after move distance"
  return (Move dir n) <?> "move command (FORW/BACK n.)"

-- | Vrida-kommandon (antar att dirMod är definierad tidigare i din kod)
parseTurn :: Parser AST
parseTurn = do
  -- Antag att dirMod definieras här
  dirMod <- matchToken $ \case
    LEFT  -> Just 1
    RIGHT -> Just (-1)
    _     -> Nothing
  n <- matchToken (\case DECIMAL x -> Just x; _ -> Nothing) <?> "decimal after LEFT/RIGHT"
  _ <- matchToken (\t -> if t == PERIOD then Just () else Nothing) <?> "period after turn angle"
  -- Använd dirMod för att skapa rätt AST-nod
  return (if dirMod == 1 then TurnLeft n else TurnRight n) <?> "turn command (LEFT/RIGHT n.)"

-- | PenUp / PenDown
parsePen :: Parser AST
parsePen = do
  -- Lägg till matchning för UP/DOWN för att definiera p
  p <- matchToken $ \case
    UP   -> Just PenUp
    DOWN -> Just PenDown
    _    -> Nothing
  _ <- matchToken (\t -> if t == PERIOD then Just () else Nothing) <?> "period after UP/DOWN"
  return p <?> "pen command (UP./DOWN.)"

-- | ChangeColor (oförändrad från ditt exempel)
parseColor :: Parser AST
parseColor = do
  _   <- matchToken (\t -> if t == COLOR then Just () else Nothing)
  hex <- matchToken (\case HEX code -> Just code; _ -> Nothing) <?> "hex code after COLOR"
  _   <- matchToken (\t -> if t == PERIOD then Just () else Nothing) <?> "period after hex code"
  return (ChangeColor hex) <?> "color command (COLOR #xxxxxx.)"

parseLoopQuoted = do
  _ <- matchToken (\case REP_KW -> Just (); _ -> Nothing)
  n <- matchToken (\case DECIMAL x -> Just x; _ -> Nothing) <?> "decimal after REP"
  _ <- matchToken (\case OPEN_QUOTE -> Just (); _ -> Nothing) <?> "opening quote (\") after REP n"
  body <- many parseCommand
  _ <- matchToken (\case CLOSE_QUOTE -> Just (); _ -> Nothing) <?> "closing quote (\")"
  return (Loop n body) <?> "quoted loop (REP n \"...\")"

parseLoopUnquoted = do
  _ <- matchToken (\case REP_KW -> Just (); _ -> Nothing)
  n <- matchToken (\case DECIMAL x -> Just x; _ -> Nothing) <?> "decimal after REP"
  cmd <- parseCommand <?> "command after REP n"
  return (Loop n [cmd]) <?> "unquoted loop (REP n <command>)"

parseLoop = (try parseLoopQuoted <|> parseLoopUnquoted) <?> "REP command"

parseCommand =  (try parseMove
            <|> try parseTurn
            <|> try parsePen
            <|> try parseColor
            <|> parseLoop) <?> "command"
-- | Hela programmet
parseProgram :: Parser [AST]
parseProgram = many parseCommand <* eof