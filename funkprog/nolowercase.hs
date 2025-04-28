noLowercaseStrings :: [String] -> [String]
noLowercaseStrings [] = []
noLowercaseStrings (x:xs)
        | x /= filter (`notElem` ['a'..'z']) x = noLowercaseStrings xs
        | otherwise = x : noLowercaseStrings xs

