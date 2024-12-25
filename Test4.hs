import Data.List (intercalate)


--First task
pairList :: [Char] -> [[Char]]
pairList [] = []
pairList xs = (take 2 xs) : (pairList (drop 2 xs)) 
--End first task



data AATree a = Empty | Node a Int (AATree a) (AATree a)

instance Show a => Show (AATree a) where
  show x = unlines (draw x)

showNode x level = [show x ++ "\t\t(" ++ show level ++ ")"]

draw :: Show a => AATree a -> [String] 
draw Empty = ["nil"]
draw (Node x level Empty Empty) = showNode x level
draw (Node x level left right)  = showNode x level ++ partright (draw right) ++ partleft (draw left)
    where partleft  = zipWith (++) ("|": repeat " ")
          partright = zipWith (++) ("|": repeat "| ")

leaf :: a -> AATree a
leaf x = Node x 1 Empty Empty

skew :: AATree a -> AATree a
skew (Node a level (Node left levelL leftL' rightL) right)
  | levelL == level = Node left levelL leftL' (Node a level rightL right)
skew x = x

split :: AATree a -> AATree a
split (Node a level left (Node right levelR leftR rightright))
  | levelR == level =
    let (Node _ levelRR _ _) = rightright
    in Node right (levelR+1) (Node a level left leftR) rightright
split x = x

insert :: Ord a => a -> AATree a -> AATree a
insert x Empty = leaf x
insert x (Node a level left right)
  | x == a = Node a level left right
  | x < a  = split (skew (Node a level (insert x left) right))
  | x > a  = split (skew (Node a level left (insert x right)))
  | otherwise = error "Can't match data types"

main :: IO ()
main = do

--First task
  putStrLn "First task"
  let charList = ['A'..'R']
  print $ charList
  print $ pairList charList
--End first task
  putStrLn "End first task"


  putStrLn "Starting Array"
  print [1..8]
  putStrLn "AA-Tree"
  let tree = foldr insert Empty (reverse [1..12])
  print tree
