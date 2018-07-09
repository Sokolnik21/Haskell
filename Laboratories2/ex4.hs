isPalindrome :: [Char] -> Bool
isPalindrome s
  | s == reverse s  = True
  | otherwise       = False

-- Collection pipeline design pattern!
getElemAtIdx :: [a] -> Int -> a
getElemAtIdx col ide =
  let col1 = drop (ide - 1) col
  in let col2 = head col1
  in col2

capitalizeHead :: Char -> Char
capitalizeHead c
  | (fromEnum c >= fromEnum 'a') && (fromEnum c <= fromEnum 'z')
    = toEnum $ (fromEnum c) + ((fromEnum 'A') - (fromEnum 'a'))
  | otherwise = c

capitalize :: [Char] -> [Char]
capitalize w = -- capitalize "ala" = "Ala"
  head1 : tail w
  where head1 = capitalizeHead (head w)
