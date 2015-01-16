import System.Environment (getArgs)

-- ex.1
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeHeadTest = do
	print $ safeHead [1]
	print $ safeHead [1..100]
--	print $ safeHead []  -- runs in ghci


safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeTailTest = do
	print $ safeTail [1]
	print $ safeTail [1..10]
--	print $ safeTail []  -- runs in ghci


safeLast :: [a] -> Maybe a
safeLast [x] = Just x
safeLast (_:xs) = safeLast xs
safeLast _ = Nothing

safeLastTest = do
	print $ safeLast [1]
	print $ safeLast [1..10]
--	print $ safeLast []  -- runs in ghci


safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit (x:xs) = Just(hh (x:xs))
	where
		hh [x] = []
		hh (x:xs) = x:(hh xs)

safeInitTest = do
	print $ safeInit [1]
	print $ safeInit [1..10]
--	print $ safeInit []  -- runs in ghci


-- ex.2
splitWith :: (a -> Bool)-> [a] -> [[a]]
splitWith f (x:xs)
	| f x =
		let (a,b) = span f (x:xs)
		in [a] ++ (splitWith f b)
	| otherwise = [[x]] ++ (splitWith f xs)
splitWith _ _  = []

splitWithTest = do
	print $ splitWith even [2,4,6,6,8,7,7,7,8]
	print $ splitWith odd [2,2,3,4,5,5,6,7,8,9]
	print $ splitWith even []


-- ex.3
interactWith f inputFile outputFile = do
	input <- readFile inputFile
	writeFile outputFile (f input)

mainHelper = mainWith myFunc
	where
		mainWith f = do
			args <- getArgs
			case args of
				[input, output] -> interactWith f input output
				_ -> putStrLn "error: exactly two arguments needed"
		-- replace "id" with the name of your function below
	--	myFunc = firstWord
		myFunc = transpose

looper :: ([a] -> b) -> [[a]] -> [b]
looper f (x:xs) = (f x):(looper f xs)
looper _ _ = []

firstWord :: String -> String
firstWord x
	| null (concat (lines x)) = []  -- stop case for `head`
	| otherwise = looper head (lines x)


-- ex.4
transpose :: String -> String
transpose x
	| null (concat (lines x)) = []  -- stop case for `tail`
	| otherwise = firstWord x ++ "\n" ++ transpose (unlines (looper tail (lines x)))


main = do
	mainHelper
