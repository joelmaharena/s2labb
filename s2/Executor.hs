-- app/Executor.hs
module Executor (executeProgram, Segment) where

import Parser (AST(..), Direction(..))

type Point   = (Double, Double)
type Segment = (String, Point, Point)

data ExecState = ExecState
  { pos   :: Point
  , angle :: Double
  , pen   :: Bool
  , color :: String
  } deriving (Show)

initialState :: ExecState
initialState = ExecState (0,0) 0 False "0000FF"

executeProgram :: [AST] -> [Segment]
executeProgram asts = snd (exec asts initialState [])
  where
    exec [] st segs = (st, segs)
    exec (c:cs) st segs = case c of
      Move dir d     ->
        let (st',segs') = move dir d st segs
        in exec cs st' segs'
      TurnLeft deg   -> exec cs (st { angle = angle st + fromIntegral deg }) segs
      TurnRight deg  -> exec cs (st { angle = angle st - fromIntegral deg }) segs
      PenDown        -> exec cs (st { pen = True }) segs
      PenUp          -> exec cs (st { pen = False }) segs
      ChangeColor c  -> exec cs (st { color = c }) segs
      Loop n body    ->
        let (st',segs') = loop n body st segs
        in exec cs st' segs'

    loop 0 _ st segs = (st, segs)
    loop k body st segs =
      let (st',segs') = exec body st segs
      in loop (k-1) body st' segs'

    move :: Direction -> Int -> ExecState -> [Segment] -> (ExecState, [Segment])
    move dir dist st segs =
      let rad     = angle st * pi / 180
          step    = fromIntegral dist
          (dx,dy) = case dir of
            DirForw -> (cos rad, sin rad)
            DirBack -> (cos (rad+pi), sin (rad+pi))
          (x,y)   = pos st
          np      = (x + dx*step, y + dy*step)
          segs'   = if pen st then segs ++ [(color st, (x,y), np)] else segs
      in (st { pos = np }, segs')

