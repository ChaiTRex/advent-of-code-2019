import qualified Data.Map as M
import Control.Applicative  (liftA2)
import Control.Monad        (join)
import Data.List            (foldl')
import Data.Maybe           (catMaybes, fromMaybe, listToMaybe)

parse :: String -> (String, String)
parse xs = (takeWhile (/= ')') xs, tail (dropWhile (/= ')') xs))

constructTree :: [(String, String)] -> M.Map String [String]
constructTree = foldl' (\ m (k, v) -> M.alter (\ vs -> Just (v:fromMaybe [] vs)) k m) M.empty

descendantCount :: M.Map String [String] -> String -> Int
descendantCount tree k = let children = fromMaybe [] (M.lookup k tree)
                         in  sum . (length children :) . map (descendantCount tree) $ children

descendantLevel :: M.Map String [String] -> String -> String -> Maybe Int
descendantLevel tree parent descendant =
  if parent == descendant then Just 0
  else                         fmap (+1) . join . fmap (listToMaybe . catMaybes . map (flip (descendantLevel tree) descendant)) $ M.lookup parent tree

shortestTrip :: M.Map String [String] -> String -> String -> Maybe Int
shortestTrip tree k1 k2 = let ps = M.keys tree
                              ts = catMaybes . map (\ p -> liftA2 (+) (descendantLevel tree p k1) (descendantLevel tree p k2)) $ ps
                          in  if ts == [] then Nothing
                                          else Just (minimum ts)

main :: IO ()
main = do
  tree <- fmap (constructTree . map parse . lines) getContents
  print . sum . map (descendantCount tree) . M.keys $ tree
  print . fmap (subtract 2) $ shortestTrip tree "YOU" "SAN"
