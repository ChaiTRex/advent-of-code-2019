fuelRequirement :: Integer -> Integer
fuelRequirement x = x `div` 3 - 2

main :: IO ()
main = do
  xs <- fmap (map read . lines) getContents
  print . sum . filter (> 0) . map fuelRequirement $ xs
  print . sum . map (sum . drop 1 . takeWhile (> 0) . iterate fuelRequirement) $ xs
