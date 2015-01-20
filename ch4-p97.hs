import Data.Char
import Data.List

-- ex.1
asInt_fold1 :: String -> Int
asInt_fold1 x = foldl' helper 0 x
	where
		helper acc x = acc * 10 + (digitToInt x)

asInt_fold1_Test = do
	-- positive number only
	print $ asInt_fold1 "0"
	print $ asInt_fold1 "101"
	print $ asInt_fold1 "1798"
	print $ asInt_fold1 ""


-- ex.2
asInt_fold2 :: String -> Int
asInt_fold2 [] = 0
asInt_fold2 x
	| head x == '-' = (asInt_fold2 (tail x)) * (-1)
	| otherwise = foldl' helper 0 x
	where
		helper acc x = acc * 10 + digitToInt x

asInt_fold2_Test = do
	print $ asInt_fold2 "0"
	print $ asInt_fold2 "101"
	print $ asInt_fold2 "-31337"
	print $ asInt_fold2 "1798"
	print $ asInt_fold2 ""
	print $ asInt_fold2 "-"
--	print $ asInt_fold2 "1f"  -- this is not vaild input since we are limited to 10-base


-- ex.3
asInt_fold3 :: String -> Int
asInt_fold3 x
	| null x = error "empty"
	| x == "-" = error "negative empty"
	| x == "-3" = error "negative three"
	| x == "314159265358979323846" = error "number pi"
	| elem '.' x = error "decimal point"
	| head x == '-' = (asInt_fold3 (tail x)) * (-1)
	| otherwise = foldl' helper 0 x
	where
		helper acc x = acc * 10 + digitToInt x

asInt_fold3_Test = do
	print $ asInt_fold3 "0"
	print $ asInt_fold3 "101"
	print $ asInt_fold3 "-31337"
	print $ asInt_fold3 "1798"

--	print $ asInt_fold3 ""
--	print $ asInt_fold3 "-"
--	print $ asInt_fold3 "-3"
--	print $ asInt_fold3 "2.7"
--	print $ asInt_fold3 "314159265358979323846"


-- ex.4
type ErrorMessage = String
data Ei = Rightt Int | Leftt ErrorMessage
	deriving (Show)

asInt_either :: String -> Ei
asInt_either x
	| null a = Rightt (asInt_fold2 x)
	| otherwise = Leftt ("non-digit \'" ++ (head a) : "\'")
	where
		a = dropWhile isHexDigit (dropWhile (== '-') x)

asInt_either_Test = do
	print $ asInt_either "0"
	print $ asInt_either "101"
	print $ asInt_either "-31337"
	print $ asInt_either "1798"
	print $ asInt_either ""
	print $ asInt_either "-"
	print $ asInt_either "foo"

	-- fuzzy test below
	print $ asInt_either "1f"  -- see `asInt_fold2_Test`
	print $ asInt_either "123g"
	print $ asInt_either "-f"
	print $ asInt_either "g"
	print $ asInt_either "-g"


-- ex.5
--wot!? see ex.6


-- ex.6
concat_fold :: [[a]] -> [a]
concat_fold x = foldr (++) [] x

concat_fold_Test = do
	print $ concat_fold [""]
	print $ concat_fold ["",""]
	print $ concat_fold ["","def"]
	print $ concat_fold ["abc",""]
	print $ concat_fold ["abc","def"]
	print $ concat_fold [[1],[2],[3]]
	print $ concat_fold [[1],[],[3]]


-- ex.7
takeWhileRe :: (a -> Bool) -> [a] -> [a]
takeWhileRe _ [] = []
takeWhileRe f (x:xs)
	| f x = x:(takeWhileRe f xs)
	| otherwise = []

takeWhileFold :: (a -> Bool) -> [a] -> [a]
takeWhileFold _ [] = []
takeWhileFold f x = foldr helper [] x
	where helper x acc
		| f x = x:acc
		| otherwise = []

takeWhileTest = do
	print $ func odd [1,3,5,8]
	print $ func odd [1..10]
	print $ func even [2,4,6,8]
	print $ func odd []
	where
	--	func = takeWhile
	--	func = takeWhileRe
		func = takeWhileFold


-- ex.8
--wot!? see ex.9


-- ex.9
-- using foldr is not natural...
groupByFoldl :: (a -> a -> Bool) -> [a] -> [[a]]
groupByFoldl _ [] = []
groupByFoldl f x = foldl' helper [[]] x
	where
		helper acc x  -- acc : [[a]]
			| null (last acc) = [[x]]
			| f (head (last acc)) x = (init acc) ++ [(last acc) ++ [x]]
			| otherwise = acc ++ [[x]]

groupByFoldTest = do
	print $ func cmp [1]
	print $ func cmp [1,3]
	print $ func cmp [1,3,0]
	print $ func cmp [1,3,5,8]
	print $ func cmp [1,3,5,9]
	print $ func cmp [1..10]
	print $ func cmp []
	where
	--	cmp = (\ x y -> (x*y) `mod` 3 == 0 )
		cmp = (\ x y -> even x )
	--	func = groupBy
		func = groupByFoldl


-- ex.10
-- see more: http://www.haskell.org/haskellwiki/Foldr_Foldl_Foldl%27

-- only foldr is applicable, because which has lazy evaluation so that it's possible to handle infinite list
anyFoldr :: (a -> Bool) -> [a] -> Bool
anyFoldr f xs = foldr helper False xs
	where
		helper x acc
			| f x = True
			| otherwise = acc

anyFoldTest = do
	print $ func ( > 100 ) [1..10]
	print $ func ( 1 == ) [1..10]
	print $ func even [1..10]
	print $ func ( > 100 ) []
	print $ func null []
	print $ func ( > 100 ) [2..]  -- infinite list
	where
	--	func = any
		func = anyFoldr


-- all 3 folds are applicable. however, foldl' is better, because it uses strict evaluation so that haskell only have to expand out the expression one at a time
cycleFoldl :: [a] -> [a]
cycleFoldl x = foldl' (++) x [cycleFoldl x]

cycleFoldr :: [a] -> [a]
cycleFoldr x = foldr (++) (cycleFoldr x) [x]

cycleFoldTest = do
	print $ func [1..5]
--	print $ func []
	where
	--	func = cycle
	--	func = cycleFoldr
		func = cycleFoldl


-- all 3 folds are applicable. however, foldr is better, because then we can use cheap (:) instead of the expensive (++), and foldl variants create too much temporary strings while computing
wordsFoldl :: String -> [String]
wordsFoldl xs = foldl' noNull [] (foldl' helper [""] xs)
	where
		helper acc x  -- acc :: [String]
			| isSpace x = acc ++ [""]
			| otherwise = (init acc) ++ [(last acc) ++ [x]]
		noNull acc x  -- filter out null strings
			| null x = acc
			| otherwise = acc ++ [x]

wordsFoldr :: String -> [String]
wordsFoldr xs = foldr noNull [] (foldr helper [""] xs)
	where
		helper x acc  -- acc :: [String]
			| isSpace x = []:acc
			| otherwise = (x:(head acc)):(tail acc)
		noNull x acc  -- filter out null strings
			| null x = acc
			| otherwise = x:acc

wordsFoldTest = do
	print $ func ""
	print $ func "  "
	print $ func "h"
	print $ func "hello"
	print $ func " hello "
	print $ func "hello world "
	where
	--	func = words
	--	func = wordsFoldl
		func = wordsFoldr


-- all 3 folds are applicable. however, foldl' is better, because it uses strict evaluation which can translate the function into tail-recursion and be optimized by compiler
unlinesFoldl :: [String] -> String
unlinesFoldl xs = foldl' helper "" xs
	where
		helper acc x = acc ++ x ++ "\n"

unlinesFoldr :: [String] -> String
unlinesFoldr xs = foldr helper "" xs
	where
		helper x acc = x ++ '\n' : acc

unlinesFoldTest = do
	print $ func []
	print $ func [""]
	print $ func ["",""]
	print $ func ["a",""]
	print $ func ["","b"]
	print $ func ["a","bb"]
	where
	--	func = unlines
	--	func = unlinesFoldr
		func = unlinesFoldl


main = do
	unlinesFoldTest
