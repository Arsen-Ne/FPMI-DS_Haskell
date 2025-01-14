import Data.Function (fix)

-- Наибольший общий делитель
gcd' :: Int -> Int -> Int
gcd' = fix (\f a b -> if b == 0 then a else f b (a `mod` b))

-- Бесконечный список чисел Фибоначчи
fibList :: [Integer]
fibList = fix (\f -> 0 : 1 : zipWith (+) f (tail f))

-- Число Фибоначчи по индексу
fib :: Int -> Integer
fib n = fix (\f i -> if i == 0 then 0 else if i == 1 then 1 else f (i - 1) + f (i - 2)) n

-- Сумма элементов списка
sumList :: [Int] -> Int
sumList = fix (\f lst -> case lst of
                            [] -> 0
                            x:xs -> x + f xs)

-- Наименьший элемент в списке
minList :: [Int] -> Int
minList = fix (\f lst -> case lst of
                            [] -> maxBound :: Int
                            [x] -> x
                            (x:xs) -> min x (f xs))

-- Реверс списка
reverseList :: [a] -> [a]
reverseList = fix (\f lst -> case lst of
                                [] -> []
                                (x:xs) -> reverseList xs ++ [x])

main :: IO ()
main = do
  putStrLn "\nНаибольший общий делитель 56 b 98:" 
  print $ gcd' 56 98 

  putStrLn "\nБесконечный список чисел Фибоначчи(10)"
  print $ take 10 fibList  

  putStrLn "\n Число Фибоначчи по индексу(10)"
  print $ fib 10  

  putStrLn "\n Сумма элементов списка [1, 2, 3, 4, 5]"
  print $ sumList [1, 2, 3, 4, 5]  

  putStrLn "\n Наименьший элемент в списке [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]"
  print $ minList [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]  

  putStrLn "\nРеверс списка [1, 2, 3, 4, 5]"
  print $ reverseList [1, 2, 3, 4, 5] 
