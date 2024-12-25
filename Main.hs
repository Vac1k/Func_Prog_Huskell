import Data.List
--problem1 start
intToWords :: (Num a, Show a) => a -> String
intToWords x = intToStr (show x)
  where
    intToStr [] = ""
    intToStr ('-':rest) = "minus-" ++ intToStr rest
    intToStr (y:rest)
      | null rest = numInWords!!(read [y] :: Int)
      | otherwise = numInWords!!(read [y] :: Int) ++ "-" ++ intToStr rest
    numInWords = ["zero",
                  "one",
                  "two",
                  "three",
                  "four",
                  "five",
                  "six",
                  "seven",
                  "eight",
                  "nine"]

problem1 = do
  print $ intToWords  150  -- "one-five-zero"
  print $ intToWords    0  -- "zero"
  print $ intToWords (-10) -- "minus-one-zero"
-- problem1 end
--problem2 start
findMaxFrequency :: (Ord a) => [a] -> (a, Int)
findMaxFrequency lst =
  if null lst
    then error "error"
    else let groups = group $ sort lst
             result = maximumBy (\x y -> compare (snd x) (snd y)) $ map (\xs -> (head xs, length xs)) groups
         in result
problem2 = do
  print $ findMaxFrequency [1,2,1,3,1,4]   -- (1, 3)
  print $ findMaxFrequency [1,1,2,2]       -- (1, 2) or (2, 2)
  print $ findMaxFrequency "some sentence" -- ('e', 4)
  print $ findMaxFrequency ([] :: [Int])   -- error
  --problem2 end
  --problem3 start
type Name   = String
type Path   = String
data FSNode = File Name | Dir Name [FSNode]

 
search :: Name -> FSNode -> [Path]
search name (File fileName)
  | name == fileName = ["/" ++ name]
  | otherwise = []
search name (Dir dirName nextStage) =
  let resultSearch = concatMap (search name) nextStage
  in if null resultSearch then [] else map (\res -> "/" ++ dirName ++ res) resultSearch


      

root = Dir "/"
  [
    Dir "folder1"
    [
      File "file1",
      Dir  "folder2"
      [
        File "file2",
        File "file3"
      ],
      Dir  "folder3"
      [
        File "file3",
        File "file4"
      ],
      File "file5"
    ]
  ]

problem3 = do
  print $ search "file1" root -- ["//folder1/file1"]
  print $ search "file3" root -- ["//folder1/folder2/file3", "//folder1/folder3/file3"]
  print $ search "file4" root -- ["//folder1/folder3/file4"]
  print $ search "file6" root -- []

main = do
  putStrLn "First Problem:"
  problem1
  putStrLn "Third Problem:"
  problem3
  putStrLn "Second Problem:"
  problem2
 