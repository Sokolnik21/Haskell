-- guards, finally

-- so, in guards there is actually a possibility that function'll throw an exception
absIntPrimitive :: Int -> Int
absIntPrimitive n | n > 0 = n
                  | n < 0 = -n

absInt :: Int -> Int
absInt n  | n > 0     = n
          | otherwise = -n

sgn :: Int -> Int
sgn n | n < 0     = -1
      | n == 0    = 0
      | otherwise = 1

min2Int :: (Int, Int) -> Int
min2Int (a, b)  | a < b     = a
                | otherwise = b

min3Int :: (Int, Int, Int) -> Int -- min (1,2,3)=1, min (1,1,3)=1
min3Int (a, b, c) | a < b     = min2Int(a, c)
                  | otherwise = min2Int(b, c)

toUpper :: Char -> Char
toUpper c | (fromEnum c) <= (fromEnum 'z') && (fromEnum c) >= (fromEnum 'a')  = toEnum ((fromEnum c) - 32)
          | otherwise = c

toLower :: Char -> Char
toLower c | (fromEnum c) <= (fromEnum 'Z') && (fromEnum c) >= (fromEnum 'A')  = toEnum ((fromEnum c) + 32)
          | otherwise = c

isDigit :: Char -> Bool
isDigit c | (fromEnum c) <= (fromEnum '9') && (fromEnum c) >= (fromEnum '0')  = True
          | otherwise = False

charToNum :: Char -> Int
charToNum c | True = (fromEnum c) - (fromEnum '0')

-- baseString, quantity, sign
addTailToRomanDigit :: (String, Int, String) -> String
addTailToRomanDigit (str, i, sign)
  | i == 0    = str
  | otherwise = addTailToRomanDigit (str ++ sign, i - 1, sign)

romanDigit :: Char -> String -- only for 1..9
romanDigit c
  | n == 9    = "IX"
  | n == 4    = "IV"
  | n < 5     = addTailToRomanDigit("", n, "I")
  | n >= 5    = addTailToRomanDigit("V", n - 5, "I")
  | otherwise = "Error"
  where n = charToNum(c)
