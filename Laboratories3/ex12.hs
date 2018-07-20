import Data.Char
import Data.List

capitalize :: [Char] -> [Char]
capitalize []   = []
capitalize (x:xs)   = toUpper x : (map toLower xs)

formatStr s =
    foldr1 (\w s -> w ++ " " ++ s) .
    map capitalize .
    filter (\x -> length x > 1) $
    words s

-- Refactored code
products = [
    ("A", 100),
    ("B", 500),
    ("C", 1000)]
    
prodPrices p =
    let res = filter (\x -> fst x == p) products in
    if res == [] then error "Unknown product"
    else
        snd $
        head res

-- basic discount strategy
discStr1 p
    | price > 999   = 0.3 * price
    | otherwise     = 0.1 * price
    where price = prodPrices p

-- flat discount strategy
discStr2 p          = 0.2 * price
    where price = prodPrices p

-- discount includes all products with price above 499
totalDiscount discStr =
    foldl1 (+) .
    map discStr .
    filter (\p -> prodPrices p > 499)