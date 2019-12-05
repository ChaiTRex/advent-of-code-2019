import Data.Char  (isDigit)
import Data.List  (groupBy, sort)

data Movement = U Int
              | D Int
              | R Int
              | L Int
  deriving (Show, Eq, Ord)

parse []       = []
parse (',':xs) = parse xs
parse ('U':xs) = (U . read . takeWhile isDigit $ xs):(parse . dropWhile isDigit $ xs)
parse ('D':xs) = (D . read . takeWhile isDigit $ xs):(parse . dropWhile isDigit $ xs)
parse ('R':xs) = (R . read . takeWhile isDigit $ xs):(parse . dropWhile isDigit $ xs)
parse ('L':xs) = (L . read . takeWhile isDigit $ xs):(parse . dropWhile isDigit $ xs)

wirePlaces :: [Movement] -> [(Int, Int, Int)]
wirePlaces = map head . groupBy (\ (x1, y1, _) (x2, y2, _) -> x1 == x2 && y1 == y2) . sort . go (0, 0, 0)
  where
    go :: (Int, Int, Int) -> [Movement] -> [(Int, Int, Int)]
    go _ []     = []
    go p (m:ms) = let (p', ws) = move p m
                  in ws ++ go p' ms
    
    move :: (Int, Int, Int) -> Movement -> ((Int, Int, Int), [(Int, Int, Int)])
    move (x, y, d) (U n) = ((x, y + n, d + n), zip3 (repeat x)              [y + 1 .. y + n]        [d + 1 .. d + n])
    move (x, y, d) (D n) = ((x, y - n, d + n), zip3 (repeat x)              [y - 1, y - 2 .. y - n] [d + 1 .. d + n])
    move (x, y, d) (R n) = ((x + n, y, d + n), zip3 [x + 1 .. x + n]        (repeat y)              [d + 1 .. d + n])
    move (x, y, d) (L n) = ((x - n, y, d + n), zip3 [x - 1, x - 2 .. x - n] (repeat y)              [d + 1 .. d + n])  

findMatches :: Ord b => (a -> b) -> [a] -> [a] -> [(a, a)]
findMatches f []         _          = []
findMatches f _          []         = []
findMatches f xxs@(x:xs) yys@(y:ys) = if      f x == f y then (x, y):findMatches f xs  ys
                                      else if f x <  f y then        findMatches f xs  yys
                                      else                           findMatches f xxs ys

manhattanDistance :: (Int, Int, Int) -> Int
manhattanDistance (x, y, _) = abs x + abs y

wireDistance :: (Int, Int, Int) -> (Int, Int, Int) -> Int
wireDistance (_, _, d1) (_, _, d2) = d1 + d2

main :: IO ()
main = do
  [xs, ys] <- fmap (map (wirePlaces . parse) . lines) getContents
  let ms = findMatches (\ (x, y, d) -> (x, y)) xs ys
  print . minimum . map (manhattanDistance . fst) $ ms
  print . minimum . map (uncurry wireDistance) $ ms

