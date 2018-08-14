actSeq = putChar 'A' >> putChar 'G' >> putChar 'H' >> putChar '\n'

doActSeq = do
  putChar 'A'
  putChar 'G'
  putChar 'H'
  putChar '\n'

doActSeq' = do
  { putChar 'A'
  ; putChar 'G'
  ; putChar 'H'
  ; putChar '\n' }

echo1 = getLine >>= putStrLn

doEcho1 = do
  line <- getLine
  putStrLn line

echo2 = getLine >>= \line -> putStrLn $ line ++ "!"

doEcho2 = do
  line <- getLine
  putStrLn $ line ++ "!"

echo3 :: IO ()
echo3 = getLine >>= \l1 -> getLine >>= \l2 -> putStrLn $ l1 ++ l2

dialog :: IO ()
dialog =  putStr "What is your happy number? "
          >> getLine
          >>= \n -> let num = read n :: Int in
                    if num == 7
                      then putStrLn "Ah, lucky 7!"
                      else if odd num
                        then putStrLn "Odd number! That's most people's choice..."
                        else putStrLn "Hmm, even number? Unusual!"

doEcho3 :: IO ()
doEcho3 = do
  l1 <- getLine
  l2 <- getLine
  putStrLn (l1 ++ l2)

doDialog :: IO ()
doDialog = do
  putStr "What is your happy number? "
  line <- getLine
  let num = read line
  if num == 7
    then putStrLn "Ah, lucky 7!"
    else if odd num
      then putStrLn "Odd number! That's most people's choice..."
      else putStrLn "Hmm, even number? Unusual!"

twoQuestions :: IO ()
twoQuestions =
  putStr "What is your name? " >>
  getLine >>= \name ->
  putStr "What is your age? " >>
  getLine >>= \age ->
  print (name,age)

getLine' :: IO ()
getLine' = loopRead []
  where
    loopRead x = do
    a <- getChar
    if a /= '\n'
      then loopRead (x ++ [a])
      else putChar '\"' >> loopWrite x
        where
          loopWrite [] = putChar '\"' >> putChar '\n'
          loopWrite (x:xs) = do
            putChar x
            loopWrite xs

getLine'' :: IO String
getLine'' = do
  x <- getChar
  if x == '\n'
    then return []
    else do
      xs <- getLine''
      return (x:xs)

getLine''' :: IO String
getLine''' = loopRead []
  where
    loopRead x = do
    a <- getChar
    if a /= '\n'
      then loopRead (x ++ [a])
      else return x
