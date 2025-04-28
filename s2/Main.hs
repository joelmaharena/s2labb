-- app/Main.hs
module Main where

import qualified Lexer
import qualified Parser
import qualified Executor
import Text.Parsec       (parse, sourceLine, errorPos)
import System.IO         (hSetEncoding, stdin, stdout, utf8)
import Text.Printf       (printf)

main :: IO ()
main = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  input <- getContents

  case parse Lexer.tokens "" input of
    Left err ->
      putStrLn $ "Syntaxfel på rad " ++ show (sourceLine (errorPos err))
    Right toks ->
      case parse Parser.parseProgram "" toks of
        Left err ->
          putStrLn $ "Syntaxfel på rad " ++ show (sourceLine (errorPos err))
        Right ast -> do
          let segments = Executor.executeProgram ast
          mapM_ printSegment segments

printSegment :: Executor.Segment -> IO ()
printSegment (c,(x1,y1),(x2,y2)) =
  printf "#%s %.4f %.4f %.4f %.4f\n" c x1 y1 x2 y2
