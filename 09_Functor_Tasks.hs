module Main where

import Prelude
--Наибольший общий делитель
gcd' :: Integer -> Integer -> Integer
gcd' a b
    | a == b = a
    | a > b = gcd' (a - b) b
    | otherwise = gcd' a (b - a)
--Возведение в степень за O(log n)
power :: Integer -> Integer -> Integer
power x n
    | n == 0 = 1
    | n `mod` 2 == 0 = power (x * x) (n `div` 2)
    | otherwise = x * power (x * x) (n `div` 2)

--Вычисление чисел Фибоначчи за O(log n)
--запутался, не осилил

    
--Совершенные числа    
isPerfect :: Integer -> Bool
isPerfect n = n == sum [x | x <- [1..n-1], n `mod` x == 0]

--Сиракузская последовательность

collatz :: Integer -> Integer
collatz n = collatz' n 1
    where
        collatz' 1 acc = acc
        collatz' n acc
            | even n = collatz' (n `div` 2) (acc + 1)
            | otherwise = collatz' (3 * n + 1) (acc + 1)

--Числа Деланнуа
delannoy :: Integer -> Integer -> Integer
delannoy 0 n = 1
delannoy m 0 = 1
delannoy m n = delannoy (m-1) n + delannoy m (n-1) + delannoy (m-1) (n-1)

--Вычисление многочлена
evalPolynomial :: Num a => [a] -> a -> a
evalPolynomial coeffs x = foldl (\acc coeff -> acc * x + coeff) 0 coeffs

--Клонирование элементов списка
clone :: Int -> [a] -> [a]
clone n xs = concatMap (replicate n) xs

--Сшивание списков бинарной операцией
xZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
xZipWith _ [] _ = []
xZipWith _ _ [] = []
xZipWith f (x:xs) (y:ys) = f x y : xZipWith f xs ys

--Фибоначчи
-- n первых чисел Фибоначчи
fibs :: Int -> [Integer]
fibs n = take n fibonacci
  where
    fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

-- Бесконечный список чисел Фибоначчи
fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

-- Обобщённые числа Фибоначчи
generalizedFibonacci :: [Integer] -> [Integer]
generalizedFibonacci initial = initial ++ rest
  where
    m = length initial
    rest = [sum $ take m $ drop i $ generalizedFibonacci initial | i <- [0..]]
    
--Системы счисления
-- Из списка цифр в число
fromDigits :: Integer -> [Integer] -> Integer
fromDigits base = foldl (\acc x -> acc * base + x) 0

-- Из числа в список цифр
toDigits :: Integer -> Integer -> [Integer]
toDigits base 0 = [0]
toDigits base n = reverse $ toDigits' n
  where
    toDigits' 0 = []
    toDigits' n = (n `mod` base) : toDigits' (n `div` base)

-- Сложение чисел в заданной системе счисления
addDigitwise :: Integer -> [Integer] -> [Integer] -> [Integer]
addDigitwise base xs ys = reverse $ normalize $ reverse $ addLists (reverse xs) (reverse ys) 0
  where
    addLists [] [] carry
      | carry == 0 = []
      | otherwise = [carry]
    addLists [] (y:ys) carry = let s = y + carry in
      (s `mod` base) : addLists [] ys (s `div` base)
    addLists (x:xs) [] carry = let s = x + carry in
      (s `mod` base) : addLists xs [] (s `div` base)
    addLists (x:xs) (y:ys) carry = let s = x + y + carry in
      (s `mod` base) : addLists xs ys (s `div` base)
    normalize = dropWhile (==0) . reverse . dropWhile (==0) . reverse
    
    
    
    
--Перечисление путей в решётке    
delannoyPaths :: Integer -> Integer -> [[Integer]]
delannoyPaths 0 0 = [[]]
delannoyPaths m n
  | m < 0 || n < 0 = []
  | otherwise = concat
    [ map (0:) (delannoyPaths (m-1) n),     
      map (1:) (delannoyPaths (m-1) (n-1)), 
      map (2:) (delannoyPaths m (n-1))      
    ]


main :: IO ()
main = do
    putStrLn "GCD examples (48 18) and (54 24):"
    print $ gcd' 48 18 
    print $ gcd' 54 24 

    putStrLn "\nPower examples (2 3) and (3 4):"
    print $ power 2 3  
    print $ power 3 4  

    putStrLn "\nPerfect numbers check 6, 28 and 12:"
    print $ isPerfect 6     
    print $ isPerfect 28    
    print $ isPerfect 12    

    putStrLn "\nCollatz sequence lengths 13 and 19:"
    print $ collatz 13  
    print $ collatz 19  

    putStrLn "\nDelannoy numbers (2 2) and (2 2): "
    print $ delannoy 2 2    
    print $ delannoy 3 3  

    putStrLn "\nEvaluate polynomial ([1, -3, 2] 3) and ([1, 0, 0, -4] 2):"
    print $ evalPolynomial [1, -3, 2] 3     
    print $ evalPolynomial [1, 0, 0, -4] 2  

    putStrLn "\nClone list elements 3 [1, 2, 3] :"
    print $ clone 3 [1, 2, 3] 

    putStrLn "\nZipWith example (+) [1, 2, 3] [4, 5, 6]:"
    print $ xZipWith (+) [1, 2, 3] [4, 5, 6] 
    
    putStrLn "\nFibonacci example (10):"
    print $ fibs 10 
    print $ take 10 fibonacci
    putStrLn "[7, 3, 10, 0]"
    print $ take 10 $ generalizedFibonacci [7, 3, 10, 0]
    putStrLn "[3, 8, 0]"
    print $ take 10 $ generalizedFibonacci [3, 8, 0]

    putStrLn "\nFrom digits (10 [1, 2, 3]) and (2 [1, 0, 1]):"
    print $ fromDigits 10 [1, 2, 3] 
    print $ fromDigits 2 [1, 0, 1]

    putStrLn "\nTo digits (10 123) and (2 5 ):"
    print $ toDigits 10 123
    print $ toDigits 2 5 

    putStrLn "\nDigitwise addition:(10 [1, 2, 3] [4, 5, 6]) and (2 [1, 0, 1] [1, 1, 1])"
    print $ addDigitwise 10 [1, 2, 3] [4, 5, 6] 
    print $ addDigitwise 2 [1, 0, 1] [1, 1, 1] 

    putStrLn "\nDelannoy paths 2 2:"
    print $ delannoyPaths 2 2
