
import Prettyify

-- ex.1
fill :: Int -> Doc -> Doc
fill width Empty = text (spaces width)
fill width Line = Line
fill width (Union l r) = Union (fill width l) (fill width r)
fill width d
    | width > docLength = Concat d (text (spaces (width - docLength)))
    | otherwise = d
    where
        docLength = count d

count :: Doc -> Int
count Empty = 0
count (Char c) = 1
count (Text t) = length t
count Line = 0
count (Union l r) = count r
count (Concat l r) = count l + count r

spaces :: Int -> String
spaces x = replicate x ' '


-- ex.2
fill2 :: Int -> Doc -> Doc
fill2 width d = undefined


main = do
    print $ fill 10 (text "asdf")
    print $ fill 10 (Concat (char 'a') (char 'b'))
