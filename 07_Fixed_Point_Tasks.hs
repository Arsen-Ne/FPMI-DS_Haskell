module Main where

import Prelude

data Pair a = Pair a a
data Labelled e a = Labelled e a
data OneOrTwo a = One a | Two a a
data MyEither a b = MyLeft a | MyRight b
data MultiTree a = Leaf | Node a [MultiTree a]
data Stream a = Cons a (Stream a)

-- 1. Pair a - пара одинаковых типов
instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

-- 2. Labelled e a - помеченное значение
instance Functor (Labelled e) where
    fmap f (Labelled e x) = Labelled e (f x)

-- 3. OneOrTwo a - один или два элемента одного типа
instance Functor OneOrTwo where
    fmap f (One x) = One (f x)
    fmap f (Two x y) = Two (f x) (f y)

-- 4. Either e a - или ошибка, или значение
instance Functor (MyEither e) where
    fmap _ (MyLeft e)  = MyLeft e
    fmap f (MyRight x) = MyRight (f x)

-- 5. MultiTree a - многоветвистое дерево
instance Functor MultiTree where
    fmap _ Leaf = Leaf
    fmap f (Node x trees) = Node (f x) (map (fmap f) trees)

-- 6. Stream a - бесконечный поток значений
instance Functor Stream where
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- список в Stream 
fromStream :: Int -> Stream Int
fromStream n = Cons n (fromStream (n + 1))

-- Stream в список
takeStream :: Int -> Stream a -> [a]
takeStream 0 _ = []
takeStream n (Cons x xs) = x : takeStream (n - 1) xs

-- Show для каждого 

instance Show a => Show (Pair a) where
    show (Pair x y) = "Pair " ++ show x ++ " " ++ show y

instance (Show e, Show a) => Show (Labelled e a) where
    show (Labelled e x) = "Labelled " ++ show e ++ " " ++ show x

instance Show a => Show (OneOrTwo a) where
    show (One x) = "One " ++ show x
    show (Two x y) = "Two " ++ show x ++ " " ++ show y

instance (Show a, Show b) => Show (MyEither a b) where
    show (MyLeft a) = "MyLeft " ++ show a
    show (MyRight b) = "MyRight " ++ show b


instance Show a => Show (MultiTree a) where
    show Leaf = "Leaf"
    show (Node x trees) = "Node " ++ show x ++ " " ++ show trees

instance Show a => Show (Stream a) where
    show (Cons x xs) = "Cons " ++ show x ++ " (" ++ show xs ++ ")"

--Для Демонстрации
testPair :: Pair Int
testPair = fmap (+1) (Pair 3 5)  

testLabelled :: Labelled String String
testLabelled = fmap (++ "!") (Labelled "Warning" "Hello") 

testOne :: OneOrTwo Int
testOne = fmap (*2) (One 5) 

testTwo :: OneOrTwo Int
testTwo = fmap (*2) (Two 3 7)

testLeft :: MyEither String Int
testLeft = fmap (*2) (MyLeft "Error") 

testRight :: MyEither String Int
testRight = fmap (*2) (MyRight 10) 

testTree :: MultiTree Int
testTree = fmap (*2) (Node 1 [Node 2 [Leaf], Node 3 [Leaf]]) 

testStream :: [Int]
testStream = takeStream 5 (fmap (+1) (fromStream 1)) 

main :: IO ()
main = do
    print testPair;     putStrLn ""
    print testLabelled; putStrLn ""
    print testOne;      putStrLn ""
    print testTwo;      putStrLn ""
    print testLeft;     putStrLn ""
    print testRight;    putStrLn ""
    print testTree;     putStrLn ""
    print testStream;   putStrLn ""
