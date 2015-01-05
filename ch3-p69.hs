import Data.List


-- ex.1
lengthh [] = 0
lengthh (x:xs) = 1 + lengthh(xs)


lengthhTest = do
	print $ length  []
	print $ lengthh []
	print $ length  [1,2,3,4,5,6,78]
	print $ lengthh [1,2,3,4,5,6,78]
	print $ length  [1..100]
	print $ lengthh [1..100]


-- ex.2
lengthh :: [a] -> Int


-- ex.3
mean :: [Double] -> Double
mean [] = 0.0
mean x = summ x / fromIntegral( len x )
	where
		summ :: [Double] -> Double
		summ [] = 0.0
		summ (x:xs) = x + summ xs

		len :: [a] -> Int
		len [] = 0
		len (x:xs) = 1 + len xs


lastt :: [a] -> a
lastt (x:xs)
	| null xs = x
	| otherwise = lastt xs


-- ex.4
palindrome :: [a] -> [a]
palindrome [] = []
palindrome (x:xs) = ( x : palindrome xs ++ [x] )


-- ex.5
ifp :: (Eq a) => [a] -> Bool
ifp [] = False
ifp x
	| (x == rev x) = True
	| otherwise = False
	where 
		rev :: [a] -> [a]
		rev [] = []
		rev (x:xs) = rev xs ++ [x]


-- ex.6
sortList :: [[a]] -> [[a]]
sortList = sortBy cmpp
	where
		cmpp :: [a] -> [a] -> Ordering
		cmpp a b
			| length a > length b = GT
			| length a < length b = LT
			| otherwise = EQ


-- ex.7
interspersee :: a -> [[a]] -> [a]
interspersee _ [] = []
interspersee _ [x] = x
interspersee a (x:xs) = x ++ [a] ++ interspersee a xs


-- ex.8
data Tree a	= Node a (Tree a) (Tree a)
			| Empty
	deriving (Show)


treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ x y)
	| accX > accY = 1 + accX
	| otherwise = 1 + accY
	where
		accX = treeHeight x
		accY = treeHeight y


-- ex.9
data Direction	= Leftt
				| Rightt
				| Straightt
	deriving (Show,Eq)


-- ex.10
type Cartesian2D = (Double, Double)


whatTurn :: Cartesian2D -> Cartesian2D -> Cartesian2D -> Direction
whatTurn (p1x,p1y) (p2x,p2y) (p3x,p3y)
	| tmp_sign > 0 = Leftt
	| tmp_sign < 0 = Rightt
	| otherwise = Straightt
	where
		(v1x ,v1y) = ( p2x - p1x , p2y - p1y )
		(v2x ,v2y) = ( p3x - p2x , p3y - p2y )
		tmp_sign = v1x * v2y - v1y * v2x


whatTurnTest = do
	print $ whatTurn (0,0) (1,1) (1,0)
	print $ whatTurn (0,0) (1,1) (0,1)
	print $ whatTurn (0,0) (1,1) (2,2)
	print $ whatTurn (0,0) (0,1) (2,2)
	print $ whatTurn (0,0) (0,1) (0,2)
	print $ whatTurn (0,0) (0,1) (0,-2)


-- ex.11
whatTurnTuple :: [Cartesian2D] -> [Direction]
whatTurnTuple (x:y:z:xs) = whatTurn x y z : whatTurnTuple (y:z:xs)
whatTurnTuple _ = []


whatTurnTupleTest = do
--	print $ whatTurnTuple []
	print $ whatTurnTuple [(0,0),(0,1),(0,-2)]
	print $ whatTurnTuple [(0,0),(0,1),(0,-2),(4,-3)]
	print $ whatTurnTuple [(0,0),(0,1),(0,-2),(4,-3),(3,4),(4,3),(0,-1),(-1,-1)]


-- ex.12
grahamScan :: [Cartesian2D] -> [(Cartesian2D,Cartesian2D)] 
grahamScan ori_set = helperFunc sorted_set
	where
		-- find the initial point
		getX = map (\ (x,y) -> x )
		getY = map (\ (x,y) -> y )
		eqYSet t = filter (\ (x,y) -> t == y )

		y_value = minimum (getY ori_set)
		x_value = minimum (getX (eqYSet y_value ori_set))
		init_point = (x_value , y_value)

		-- sort the points set
		vectorr (x1,y1) (x2,y2) = (x2-x1 , y2-y1)
		removeP rp set = filter (\ x -> x /= rp) set
		dotPAngle (v1x,v1y) (v2x,v2y) = (v1x*v2x + v1y*v2y) / (sqrt(v1x*v1x + v1y*v1y) * sqrt(v2x*v2x + v2y*v2y))
		sortAngle rp set = sortBy (\ p1 p2 -> comparee ((1,0) , (vectorr rp p1)) ((1,0) , (vectorr rp p2))) set
			where
				comparee ((a1x,a1y) , (b1x,b1y)) ((c1x,c1y) , (d1x,d1y))
					-- custom sort: 1. sort by angle to reference point(rp).  2. sort by distance to reference point.
					| dotPAngle (a1x,a1y) (b1x,b1y) > dotPAngle (c1x,c1y) (d1x,d1y) = LT
					| dotPAngle (a1x,a1y) (b1x,b1y) < dotPAngle (c1x,c1y) (d1x,d1y) = GT
					| otherwise =
						compare (abs(b1x) + abs(b1y)) (abs(d1x) + abs(d1y))

		tmp_set = removeP init_point ori_set
		sorted_set = init_point : (sortAngle init_point tmp_set) ++ [init_point]

		-- main algorithm here
		helperFunc [x,y,z] = [(x,y),(y,z)]
		helperFunc (x:y:z:xs) =
			if whatTurn x y z == Leftt
			then
				if whatTurn y z (head xs) == Leftt
				then
					(x,y) : helperFunc (y:z:xs)
				else
					helperFunc (x:y:xs)
			else
				helperFunc (x:z:xs)
		helperFunc _ = []


grahamScanTest = do
	print $ grahamScan []
	print $ grahamScan [(0,0)]
	print $ grahamScan [(0,0),(4,3)]          -- minimal valid set
	print $ grahamScan [(0,0),(0,1),(0,-2)]   -- all 3 points aligned on a line, should be valid
	print $ grahamScan [(0,0),(4,3),(0,3)]
	print $ grahamScan [(0,0),(4,3),(0,3),(0,0),(4,3),(0,3),(0,0),(4,3),(0,3)]    -- point set repeated 3 times
	print $ grahamScan [(0,0),(3,0),(0,3),(3,3),(3,-3),(-3,3),(-3,-3)]
	print $ grahamScan [(0,0),(0,1),(0,-2),(4,-3),(3,4),(4,3),(0,-1),(-1,-1)]


main = do
	grahamScanTest
