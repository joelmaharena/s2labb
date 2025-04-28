removeEveryNth :: Int -> [Int] -> [Int]
removeEveryNth n v 
    | n == 1 = []
    | n > length v = []
    | otherwise = helper 1 [] v
    where
        helper :: Int -> [Int] -> [Int] -> [Int]
        helper _  acc [] = reverse acc
        helper i acc (x:xs)
            | i `mod` n == 0 = helper (i+1) acc xs
            | otherwise      = helper (i+1) (x:acc) xs

