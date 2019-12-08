import Data.Char  (isDigit)

every :: Int -> [a] -> [[a]]
every n xs = takeWhile (not . null) . go n $ xs
  where
    go :: Int -> [a] -> [[a]]
    go n xs = take n xs:every n (drop n xs)

parse :: Int -> Int -> String -> [[[Int]]]
parse cols rows = map (every cols) . every (cols * rows) . map (read . (: [])) . filter isDigit

layer :: [[[Int]]] -> [[Int]]
layer xs = foldl1 (zipWith (zipWith combine)) xs
  where
    combine :: Int -> Int -> Int
    combine 0 _ = 0
    combine 1 _ = 1
    combine 2 n = n

color :: Int -> Char
color 0 = ' '
color 1 = '*'
color 2 = 'x'

main :: IO ()
main = do
  xs <- fmap (parse 25 6) getContents
  
  let ys = snd . minimum $ zip (map (length . filter (== 0) . concat) xs) xs
  print $ (length . filter (== 1) . concat $ ys) * (length . filter (== 2) . concat $ ys)
  
  mapM_ putStrLn . map (map color) . layer $ xs
