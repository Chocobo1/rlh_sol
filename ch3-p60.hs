-- ex.1
data List	a = Cons a (List a)
			| Nil
	deriving (Show)

conFromList :: List a -> [a]
conFromList Nil = []
conFromList (Cons x xs) = x:conFromList xs

conFromListTest = do
--	print $ conFromList Nil
	print $ conFromList (Cons 0 Nil)
	print $ conFromList (Cons 3 (Cons 2 (Cons 1 (Cons 0 Nil))))


-- ex.2
data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
	deriving (Show)


dataTreeTest = do
	print $ (Node "parent" Nothing Nothing)
	print $ (Node "parent" (Just(Node "left" Nothing Nothing)) (Just(Node "right" Nothing Nothing)))


main = do
	conFromListTest
	dataTreeTest
