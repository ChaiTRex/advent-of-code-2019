import Data.Char   (isDigit, ord)
import Data.List   (group, sort)
import Data.Maybe  (catMaybes)

main :: IO ()
main = do
  gs <- fmap ( catMaybes
             . map (maybeMinimum . filter (>= 2) . map length . group)
             . filter nondecreasing
             . map show
             . uncurry enumFromTo
             . parse
             ) getContents
  print . length $ gs
  print . length . filter (== 2) $ gs

nondecreasing :: Ord a => [a] -> Bool
nondecreasing xs = and . zipWith (<=) xs $ tail xs

maybeMinimum :: Ord a => [a] -> Maybe a
maybeMinimum [] = Nothing
maybeMinimum xs = Just (minimum xs)

parse :: String -> (Integer, Integer)
parse = goLeft 0
  where
    appendDigit :: Integer -> Char -> Integer
    appendDigit x d = 10*x + toInteger (ord d - ord '0')
    
    goLeft :: Integer -> String -> (Integer, Integer)
    goLeft m ('-':xs) = goRight m 0 xs
    goLeft m (d  :xs) = goLeft (appendDigit m d) xs
    
    goRight :: Integer -> Integer -> String -> (Integer, Integer)
    goRight m n ""     = (m, n)
    goRight m n (d:xs) = if isDigit d then goRight m (appendDigit n d) xs
                                      else (m, n)

