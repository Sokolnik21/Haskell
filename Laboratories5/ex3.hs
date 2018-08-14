nTimes :: Int -> IO () -> IO ()
nTimes 0 action = return ()
nTimes n action = do
  action
  nTimes (n - 1) action

ioActionFactory :: Int -> String -> IO ()
ioActionFactory n = case n of
  1 -> \name -> putStrLn ("Good morning, " ++ name)
  2 -> \name -> putStrLn ("Good afternoon, " ++ name)
  3 -> \name -> putStrLn ("Good evening, " ++ name)
  _ -> \name -> putStrLn ("Hello, " ++ name)

actionList :: [IO ()]
actionList = [ioActionFactory 1 "Ben",
              ioActionFactory 2 "Joe",
              ioActionFactory 3 "Ally"]

sequence' :: [IO ()] -> IO ()
sequence' []      = return ()
sequence' (a:as)  = do  a
                        sequence' as

sequence'' :: [IO ()] -> IO ()
sequence'' = foldr (>>) (return ())

sequence''' :: [IO ()] -> IO ()
sequence''' = foldr (\x -> (>>) x) (return ())

sequence4 :: [IO ()] -> IO ()
sequence4 = foldr (>>) (return ()) . (\x -> reverse x)

sequence5 :: [IO ()] -> IO ()
sequence5 = foldl (\x y -> y >> x) (return ())
