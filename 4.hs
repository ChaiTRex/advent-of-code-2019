import Data.List  (group, sort)

main :: IO ()
main = do
  [a, b] <- fmap (map read . lines . map (\ ch -> if ch == '-' then '\n' else ch)) getContents
  let gs = map (sort . map length . group) . filter (\ ds -> ds == sort ds) . map show $ [a :: Integer .. b]
  print . length . filter ((> 1) . last) $ gs
  print . length . filter (elem 2) $ gs
