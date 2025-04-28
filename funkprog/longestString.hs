longestString :: [String] -> String
longestString [] = []
longestString (x:xs) = foldl (helpFunc) x xs

helpFunc :: String -> String -> String
helpFunc x y 
    | length x >= length y = x
    | otherwise = y
