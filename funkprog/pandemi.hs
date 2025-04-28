sirSimulate :: Double -> Double -> Int -> Int -> (Int, Int, Int) -> [(Int, Int, Int)]
sirSimulate beta gamma n steps initialState = 
  take (steps) $ iterate (nextState beta gamma n) initialState -- iterate applicerar funktionen oändligt många ggr på initialState

nextState :: Double -> Double -> Int -> (Int, Int, Int) -> (Int, Int, Int)
nextState beta gamma n (s, i, r) =
  let
    sD = fromIntegral s
    iD = fromIntegral i
    nD = fromIntegral n
    
    newInfections = beta *  sD * iD /  nD
    newRecoveries = gamma * iD
    
    s' = round(sD - newInfections)
    i' = round(iD + newInfections - newRecoveries)
    r' = r + round(newRecoveries)
  in
    (s', i', r')