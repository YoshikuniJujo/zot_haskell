import Data.Char

main = interact $ concatMap $ reverse . showBits 8 . ord

showBits :: Int -> Int -> String
showBits 0 d
	| d == 0	= ""
	| otherwise	= error "overflow"
showBits n d = show ( d `mod` 2 ) ++ showBits ( n - 1 ) ( d `div` 2 )
