import Data.Maybe  (catMaybes)

parse :: Int -> Int -> String -> [[String]]
parse cols rows = every rows . every cols . catMaybes . map toColor
  where
    every :: Int -> [a] -> [[a]]
    every n = takeWhile (not . null) . go n
      where
        go :: Int -> [a] -> [[a]]
        go n xs = take n xs:go n (drop n xs)

    toColor :: Char -> Maybe Char
    toColor '0' = Just ' '
    toColor '1' = Just '█'
    toColor '2' = Just '▒'
    toColor _   = Nothing

combineLayers :: [[String]] -> [String]
combineLayers = foldl1 (zipWith (zipWith combine))
  where
    combine :: Char -> Char -> Char
    combine '▒' c = c
    combine c   _ = c

main :: IO ()
main = do
  xs <- fmap (parse 25 6) getContents
  print . product . tail . minimum . map (\ y -> map (\ c -> length . filter (== c) $ y) " █▒") . map concat $ xs
  mapM_ putStrLn . combineLayers $ xs

