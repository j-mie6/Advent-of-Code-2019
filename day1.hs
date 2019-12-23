input :: IO [Int]
input = fmap (map read . lines) (readFile "day1-input.txt")

eq :: Int -> Int
eq = subtract 2 . flip div 3

task :: (Int -> Int) -> IO Int
task f = sum . map f <$> input

task1 :: IO Int
task1 = task eq
-- By fold fusion:
-- task1 = fmap (foldr (\x acc -> eq x + acc) 0) input

cascade :: Int -> Int
-- tail because the module weight does not contribute
cascade = sum . takeWhile (/= 0) . tail . iterate (max 0 . eq)

task2 :: IO Int
task2 = task cascade
