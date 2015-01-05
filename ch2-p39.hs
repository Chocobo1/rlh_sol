-- ex.1
{-
Return a element from the list.
It cannot return an empty list nor another data type which is different from the input list.
-}


-- ex.2
lastButOne :: [a] -> a
lastButOne [x,y] = x
lastButOne (x:xs) = lastButOne xs

lastButOneTest = do
	print $ lastButOne [1,2,3]
	print $ lastButOne [1..100]
	print $ lastButOne [1,2]
	print $ lastButOne [1]
--	print $ lastButOne []


-- ex.3
{-
We get an exception: Non-exhaustive patterns in function lastButOne
-}
