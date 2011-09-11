import Data.Char

main = interact $ map ( chr . bitsToInt ) . splitByN 8

splitByN :: Int -> [ a ] -> [ [ a ] ]
splitByN n [ ]	= [ ]
splitByN n xs	= take n xs : splitByN n ( drop n xs )

bitsToInt :: String -> Int
bitsToInt = bti . reverse
	where
	bti ""			= 0
	bti ( '0' : rest )	=     2 * bti rest
	bti ( '1' : rest )	= 1 + 2 * bti rest
	bti ( c : rest )
		| isSpace c	= bti rest
