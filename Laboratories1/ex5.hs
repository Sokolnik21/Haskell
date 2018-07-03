-- if ... then ... else
-- or a little misery before guards will show
sgn :: Int -> Int
sgn n =
  if n < 0
    then -1
    else if n == 0
      then 0
      else 1

absInt :: Int -> Int -- absInt 2 = absInt (-2) = 2
absInt a =
  if a >= 0
    then a
    else -a

min2Int :: (Int, Int) -> Int -- min (1,2) = 1, min (-1, -1) = -1
min2Int (a, b) =
  if a > b
    then b
    else a

min3Int :: (Int, Int, Int) -> Int
min3Int (a, b, c) =
  if a <= b
    then if a <= c
      then a
      else c
    else if b <= c
      then b
      else c

min3IntWithMin2Int :: (Int, Int, Int) -> Int
min3IntWithMin2Int (a, b, c) =
  min2Int(min2Int(a, b), c)

toUpper :: Char -> Char
toUpper c =
  if (fromEnum c) <= (fromEnum 'z') && (fromEnum c) >= (fromEnum 'a')
    then toEnum ((fromEnum c) - 32)
    else c

toLower :: Char -> Char
toLower c =
  if (fromEnum c) <= (fromEnum 'Z') && (fromEnum c) >= (fromEnum 'A')
    then toEnum ((fromEnum c) + 32)
    else c

isDigit :: Char -> Bool
isDigit c =
  if (fromEnum c) <= (fromEnum '9') && (fromEnum c) >= (fromEnum '0')
    then True
    else False

charToNum :: Char -> Int
charToNum c = (fromEnum c) - (fromEnum '0')

-- baseString, quantity, sign
addTailToRomanDigit :: (String, Int, String) -> String
addTailToRomanDigit (str, i, sign) =
  if i == 0 then str
    else addTailToRomanDigit (str ++ sign, i - 1, sign)

romanDigit :: Char -> String -- only for 1..9
romanDigit c = let n = charToNum(c) in
  if n == 9 then "IX"
    else if n == 4 then "IV"
      else if n < 5 then addTailToRomanDigit("", n, "I")
        else addTailToRomanDigit("V", n - 5, "I")
