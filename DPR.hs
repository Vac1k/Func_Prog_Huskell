-------------------------------Task 1
diff :: (Double -> Double) -> Double -> Double -> Double
diff f dx x = (f (x + dx) - f x) / dx
newton_iter :: (Double -> Double) -> (Double -> Double) -> Double -> Int -> Double
newton_iter f f' x 0 = x
newton_iter f f' x k = newton_iter f f' (x - f x / f' x) (k - 1)
task_1 = do
  let f1 = \x -> sin x
  let f2 = \x -> x ** 3 - 328 * x ** 2 - 1999 * x - 1670
  putStrLn "  1) sin(x):"
  putStrLn "\nk = 100"
  print $ newton_iter f1 (diff f1 0.01) 0.5 100
  putStrLn "\n  2) x^3 - 328 * x^2 - 1999 * x - 1670:"
  putStrLn "\nk = 100"
  print $ newton_iter f2 (diff f2 0.01) 100 100
  putStrLn "\nk = 1000"
  print $ newton_iter f2 (diff f2 0.01) 100 1000
-------------------------------Task 2

type IntSet = (Int -> Bool)

isMember :: IntSet -> Int -> Bool
isMember f x = f x

emptySet :: IntSet
emptySet x = False

allInts :: IntSet
allInts x = True

interval :: Int -> Int -> IntSet
interval lBound uBound = \x -> (x >= lBound) && (x <= uBound)

euclid :: Integer -> Integer -> Integer
euclid a 0 = a
euclid a b = euclid b (a `rem` b)

coprime :: Integer -> Integer -> Bool
coprime a b = (euclid a b) == 1

setIntersection :: IntSet -> IntSet -> IntSet
setIntersection f g = \x -> f x && g x

setUnion :: IntSet -> IntSet -> IntSet
setUnion f g = \x -> f x || g x

setComplement :: IntSet -> IntSet -> IntSet
setComplement f g = \x -> not (f x)

addToSet :: Int -> IntSet -> IntSet
addToSet x set = setUnion set (interval x x)

deleteFromSet :: Int -> IntSet -> IntSet
deleteFromSet x set = setComplement set (interval x x)

allComplement :: IntSet -> IntSet
allComplement set = setComplement allInts set

task_2 :: IO ()
task_2 = do
  putStrLn "- - - - - - - - - -\n      PART A\n"
  print $ allInts 123 -- True
  print $ emptySet 123 -- False

  putStrLn "- - - - - - - - - -\n      PART B\n"
  print $ (interval 1 100) 99 -- True
  print $ (interval 1 100) 101 -- False

  putStrLn "- - - - - - - - - -\n      PART C\n"
  print $ (coprime 18) 23 -- True
  print $ (coprime 2) 18 -- False

  putStrLn "- - - - - - - - - -\n      PART D\n"
  print $ (setIntersection (interval 10 100) (interval 80 150)) 90 -- True
  print $ (setIntersection (interval 10 100) (interval 80 150)) 110 -- False
  putStrLn "\n"
  print $ (setUnion (interval 10 100) (interval 80 150)) 70 -- True
  print $ (setUnion (interval 10 100) (interval 80 150)) 170 -- False
  putStrLn "\n"
  print $ (setComplement (interval 10 100) (interval 80 150)) 120 -- True
  print $ (setComplement (interval 10 100) (interval 80 150)) 90 -- False
  putStrLn "\n"
  print $ (addToSet 100 (interval 20 80)) 100 -- True
  print $ (addToSet 10 (interval 20 80)) 90 -- False
  putStrLn "\n"
  print $ (deleteFromSet 50 (interval 10 50)) 50 -- False
  
-------------------------------Task 3

data Operator = Add | Sub | Div | Mul | Mod | Pow deriving (Eq)

isOp :: Token -> Bool
isOp (Operator _) = True
isOp _ = False

data Parenthesis = Opened | Closed deriving (Eq)

data Token = Number Float | Operator Operator | Parenthesis Parenthesis deriving (Eq)

prio :: Token -> Int
prio (Operator Add) = 0
prio (Operator Sub) = 0
prio (Operator Mul) = 1
prio (Operator Div) = 1
prio (Operator Mod) = 2
prio (Operator Pow) = 3
prio (Parenthesis Opened) = 4
prio (Parenthesis Closed) = 4
prio (Number _) = 5

type Stack = [Token]

parse :: String -> Stack
parse string = normalize (tokenize string [])

tokenize :: String -> String -> Stack
tokenize [] [] = []
tokenize [] string = toNumber (reverse string)
tokenize (x:xs) string =
  case x of
    ' ' -> tokenize xs string
    '+' -> addToken (Operator Add)
    '-' -> addToken (Operator Sub)
    '/' -> addToken (Operator Div)
    '*' -> addToken (Operator Mul)
    '%' -> addToken (Operator Mod)
    '^' -> addToken (Operator Pow)
    '(' -> addToken (Parenthesis Opened)
    ')' -> addToken (Parenthesis Closed)
    _ -> tokenize xs (x : string)
  where
    addToken token = toNumber (reverse string) ++ [token] ++ tokenize xs []

toNumber :: String -> Stack
toNumber [] = []
toNumber string = [Number (read string :: Float)]

normalize :: Stack -> Stack
normalize input = normalize' input [] []
  where
    normalize' [] [] output = output
    normalize' [] stack output
      | head stack == Parenthesis Opened = error "Parenthesis error"
      | otherwise = normalize' [] (tail stack) (output ++ [head stack])
    normalize' (Number x : xs) stack output = normalize' xs stack (output ++ [Number x])
    normalize' (Parenthesis Opened : xs) stack output = normalize' xs (Parenthesis Opened : stack) output
    normalize' (Parenthesis Closed : xs) stack output =
      normalize' xs stack' output'
      where
        stack' = tail $ dropWhile (/= Parenthesis Opened) stack
        output' = output ++ takeWhile (/= Parenthesis Opened) stack
    normalize' (Operator o1 : xs) stack output =
      normalize' xs stack' output'
      where
        condition o2 = isOp o2 && prio (Operator o1) < prio o2
        (match, rest) = span condition stack
        stack' = Operator o1 : rest
        output' = output ++ match

eval :: Stack -> Float
eval stack = fromNumber $ head $ foldl folder [] stack
  where
    fromNumber (Number x) = x
    folder (Number x : Number y : ys) (Operator Pow) = Number (y ** x) : ys
    folder (Number x : Number y : ys) (Operator Mod) = Number (fromIntegral (mod (floor y) (floor x))) : ys
    folder (Number x : Number y : ys) (Operator Mul) = Number (y * x) : ys
    folder (Number x : Number y : ys) (Operator Div) = Number (y / x) : ys
    folder (Number x : Number y : ys) (Operator Sub) = Number (y - x) : ys
    folder (Number x : Number y : ys) (Operator Add) = Number (y + x): ys
    folder xs (Number x) = Number x : xs

-------------------------------Task 4

data Dual a = Dual a a deriving (Show)

real :: Dual a -> a
real (Dual r _) = r

dual :: Dual a -> a
dual (Dual _ d) = d

instance Eq a => Eq (Dual a) where
    (==) (Dual r1 d1) (Dual r2 d2) = (r1 == r2) && (d1 == d2)

instance Num a => Num (Dual a) where
    (+) (Dual r1 d1) (Dual r2 d2) = Dual (r1 + r2) (d1 + d2)
    (-) (Dual r1 d1) (Dual r2 d2) = Dual (r1 - r2) (d1 - d2)
    (*) (Dual r1 d1) (Dual r2 d2) = Dual (r1 * r2) (r1 * d2 + r2 * d1)
    negate (Dual r d) = Dual (negate r) (negate d)
    abs (Dual r d) = Dual (abs r) (d * (signum r))
    signum (Dual r d) = Dual (signum r) 0
    fromInteger a = Dual (fromInteger a) 0

instance Fractional a => Fractional (Dual a) where
    (/) (Dual r1 d1) (Dual r2 d2) = Dual (r1 / r2) ((d1 * r2 - r1 * d2) / r2 ^ 2)
    recip (Dual r d) = Dual (recip r) (-1 * d * (recip (r ^ 2)))
    fromRational n = Dual (fromRational n) 0

instance Floating a => Floating (Dual a) where
    pi = Dual pi 0
    (**) (Dual r1 d1) (Dual r2 d2) = Dual (r1 ** r2) (r1 ** r2 * (d2 * (log r1) + (r2 * d1 / r1)))
    sqrt (Dual r d) = Dual (sqrt r) (d / (2 * sqrt r))
    acos (Dual r d) = Dual (acos r) (- d / (sqrt(1 - r * r)))
    asin (Dual r d) = Dual (asin r) (d / (sqrt(1 - r * r)))
    atan (Dual r d) = Dual (atan r) (d / (1 + r * r))
    cos (Dual r d) = Dual (cos r) (-d * sin r)
    sin (Dual r d) = Dual (sin r) (d * cos r)
    tan (Dual r d) = Dual (tan r) (1 / ((cos r) ** 2))
    acosh (Dual r d) = Dual (acosh r) (d / sqrt(r ** 2 - 1))
    asinh (Dual r d) = Dual (asinh r) (d / sqrt(1 + r ** 2))
    atanh (Dual r d) = Dual (atanh r) (d / (1 - r ** 2))
    cosh (Dual r d) = Dual (cosh r) (d * sinh r)
    sinh (Dual r d) = Dual (sinh r) (d * cosh r) 
    tanh (Dual r d) = Dual (tanh r) (d * (1 - (tanh r) ** 2))
    exp (Dual r d) = Dual (exp r) (d * (exp r))
    log (Dual r d) = Dual (log r) (d / r)

acoth :: Floating a => a -> a
acoth x = (1/2) * log ((x + 1) / (x - 1))

acothD :: Floating a => Dual a -> Dual a 
acothD (Dual r d) = Dual (acoth r) (d / (1 - r ** 2))

derivative f x = dual (f (Dual x 1))















main = do
  putStrLn "DPR Onyshchenko Volodymyr KM-13\n"

  putStrLn "------------------------First Task---------------------\n"
  task_1
  putStrLn "------------------------Second Task--------------------\n"
  task_2
  putStrLn "------------------------Third Task---------------------\n" 

  putStrLn $ "10 * 2 ^ (3 - 1) * 3.5 = " ++ show (eval (parse "10 * 2 ^ (3 - 1) * 3.5"))
  putStrLn $ "10 / (2 % 2)) + 1 = " ++ show (eval (parse "(10 / (2 % 2)) + 1"))
  putStrLn $ "(2 + 2)) + (((3 ^ 2 % 2))) = " ++ show (eval (parse "((2 + 2)) + (((3 ^ 2 % 2)))"))
  putStrLn "------------------------Fourth Task--------------------\n" 

  let func  = \x -> sin(2 * (exp(1))**(x**2))
  let func2  = \x -> x**3 - log(x**2) + 14*cos(x/2) + (acoth (x))**2

  putStrLn $ "derivative sin(2e^(x^2)) pi = " ++ show (derivative func pi)
  putStrLn $ "\nderivative x^3 - log(x^2) + 14cos(x/2) + atanh(x)^2 pi = " ++ show(derivative func2 pi)

