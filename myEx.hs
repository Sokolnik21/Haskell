-- Here I'll place some ideas of functions that came to my mind during basic exercises

-- digitToRoman [1..3999]
data Tier = One |
            Ten |
            Hundred |
            Thousand

ones = [
  (One,       "I", 1),
  (Ten,       "X", 10),
  (Hundred,   "C", 100),
  (Thousand,  "M", 1000)]

fives = [
  (One,       "V", 5),
  (Ten,       "L", 50),
  (Hundred,   "D", 500)]

-- arabToRoman :: Int -> String
-- arabToRoman n =


romanToArab :: String -> Int
romanToArab list = loop 0 list
  where
    loop res [] = res
    loop res (x:xs) =
