
import Data.List

--encode x = let (values,lenghts) = (compress x, (map (\a -> length a) (pack x)))
--    in zip lenghts values

encode (x:xs) = let (first,rest) = span (==x) xs
    in ((length (x:first),x)) : encode rest
encode [] = []

data Item a  = Single a | Multiple (Int,a) deriving (Show)

encodeModified' x = map enc (pack x)
    where
        enc a = if length a == 1 
                    then Single (head a)
                    else Multiple (length a, head a)

encodeModified :: Eq a => [a] -> [Item a]
encodeModified = map enc . encode
    where
        enc (1,a) = Single a
        enc (n,a) = Multiple (n,a)

decodeModified x = foldl decode [] x
    where decode acc (Single a) = acc ++ [a]
          decode acc (Multiple (n, a)) = acc ++ replicate n a

encodeDirect z = encode' 1 [] z
    where 
       encode' n acc [] = acc
       encode' n acc (x:xs) = if (length xs>0 && head xs == x) 
                                    then encode' (n+1) (acc) xs 
                                    else encode' 1 (acc ++ [getItem n x]) xs
                                        where getItem 1 x = Single(x)
                                              getItem n x = Multiple(n,x)

dupli = concatMap (replicate 2)

repli x n = concatMap (replicate n) x

dropEvery x m = dropEvery' 1 [] x
        where dropEvery' n acc [] = acc
              dropEvery' n acc (y:ys) = dropEvery' (n+1) maybeAdd ys
                where maybeAdd | n `mod` m == 0 = acc
                               | otherwise = (acc ++ [y]) 

split list l z = split' 1 [] [] list
    where 
        split' n acc curr [] = acc
        split' n acc curr (x:xs) = split' (n+1) maybeAdd maybeAddCurr xs
                  where maybeAdd | n == l || n-l == z || length list == n = acc ++ [curr++[x]]
                                 | otherwise = acc
                        maybeAddCurr | n == l || n-l==z = [] 
                                     | otherwise = curr++[x]

slice list s e = take (e-s+1) (drop (s-1) list) 

rotate n list = (drop n list) ++ (take n list)

removeAt n list = (list !! (n-1),take (n-1) list ++ (drop n list))

