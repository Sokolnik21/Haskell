printHello = putStrLn "Hello"

-- Q:
-- What is needed to run this program via "runghci"
-- A:
-- Every program starts from "main" function
-- To run this program via terminal it is needed to tell computer where to start
-- And that start point "main" function itself
-- Without it program won't start (because it won't know what to do)
main = printHello
