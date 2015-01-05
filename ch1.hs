-- ex.1
a = 5+8				--Integer
b = 3*5+8			--Integer
c = 2+4				--Integer
d = (+) 2 4			--Integer
e = sqrt 16			--Double
f = succ 6			--Integer
g = succ 7			--Integer
h = pred 9			--Integer
i = pred 8			--Integer
j = sin (pi / 2)	--Double
k = truncate pi		--Integer
l = round 3.5		--Integer
m = round 3.4		--Integer
n = floor 3.7		--Integer
o = ceiling 3.3		--Integer
-- :type a


-- ex.2
-- x :: Num a => a = _


-- ex.3
countWords = interact wordCount
	where wordCount input = show (length (words input)) ++ "\n"


-- ex.4
countChar = interact wordCount
	where wordCount input = show (length input) ++ "\n"
