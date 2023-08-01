import Data.Char
import Data.String (IsString)
import Distribution.Simple.Utils (xargs)
import System.Win32 (COORD(x))

-- test programs from Haskell book
-- generate primes
factors :: Int ->[Int]
factors n = [x | x<-[1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n =[x | x <- [2..n], prime x]


-- code for Caeser cipher

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x==x']

let2int :: Char -> Int
let2int c= ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c   | isLower c = int2let ((let2int c + n) `mod` 26)
            | otherwise = c
            
encode :: Int -> String -> String
encode n xs = [shift n x | x <-xs]

table :: [Float]
table =     [8.1,1.5,2.8,4.2,12.7,2.2,2.0,6.1,7.0,
            0.2,0.8,4.0,2.4,6.7,7.5,1.9,0.1,6.0,
            6.3,9.0,2.8,1.0,2.4,0.2,2.0,0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral  n / fromIntegral m)*100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x<-['a'..'z']]
            where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [(o-e)^2/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
    where
        factor = head(positions(minimum chitab) chitab)
        chitab = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs xs 

newfunc :: Num a => a -> a
newfunc x=x+7
