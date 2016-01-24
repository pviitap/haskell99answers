
import Data.List

myLast :: [a] -> a
myLast [x] = x
myLast (_:x) = myLast x

myButLast :: [a] -> a
myButLast x = x !! (length x - 2)

elementAt :: [a] -> Int -> a
elementAt x b = x !! (b-1)

--myLength x = foldl (+) 0 [ 1 | y <- x ]

myLength :: [a] -> Int
myLength [] = 0
myLength (_:x) = 1 + myLength x

myReverse :: [a] -> [a]
myReverse [x] = [x]
myReverse x = myReverse_acc x [head x]
					where 
							myReverse_acc [x] b = b
							myReverse_acc (_:x) b = myReverse_acc x ( [head x] ++ b)


isPalindrome s = s == myReverse s

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
--flatten (List (x:xs)) = foldl (++) (flatten x) (map flatten xs)
--flatten (List (xs)) = foldl (++) (map flatten xs)
--flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List x) = concatMap flatten x

--compress [x] = [x]
--compress (x:xs) = if head xs == x then compress xs else (x : compress xs)

compress :: Eq a => [a] -> [a]
compress = map head . group

--pack x = pack_acc [[head x]] (tail x)
--	where  
--				 pack_acc acc [] = acc
--				 pack_acc acc (x:xs) = pack_acc ( if x `elem` last(acc) then addtolast else (acc ++ [[x]])) xs
--			 			where addtolast = (map (\a -> if snd(a) == length acc then (fst (a) ++ [x]) else fst a) (zip acc [1..]))

pack (x:xs) = let (first,rest) = span (==x) xs
    in (x:first) : pack rest
pack [] = []
