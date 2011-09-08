module Main where

data Fun = Fun { apply :: Fun -> Fun } | Int Int

data Bit = I | O deriving Show

instance Show Fun where
	show ( Int i ) = show i
	show ( Fun _ ) = "function"

s, k, i :: Fun
s = Fun $ \x -> Fun $ \y -> Fun $ \z -> apply ( apply x z ) ( apply y z )
k = Fun $ \x -> Fun $ \y -> x
i = Fun $ \x -> x

empty :: Fun
empty = Fun $ \c -> apply c i

zero :: Fun
zero = Fun $ \c -> apply c $ Fun $ \f -> apply ( apply f s ) k

one :: Fun
one = Fun $ \c -> Fun $ \ll -> apply ll
	$ Fun $ \l -> Fun $ \rr -> apply rr $ Fun $ \r -> apply c $ apply l r

main :: IO ()
main = do
	print testZot2
	print $ interrogate one $$ Int 0 $$ Int 1
	print $ interrogate zero $$ Int 0 $$ Int 1
	print $ interrogate output $$ Int 0 $$ Int 1 $$ Int 2
	pr one
	pr zero
	print testRev
	print $ makeZotFromStr "10100" $$ Int 3 $$ Int 4

lst :: Fun
lst = Fun $ \f -> ( \g -> i $$ ( g $$ one ) ) ( f $$ zero )

{-
readList :: Fun -> [ Fun ]
readList l = if isNull l then [ ] else
	l k
-}

testSKI :: Fun
testSKI = apply ( apply ( apply s k ) ( Int 8 ) ) $ Int 4

testZot :: Fun
testZot = apply ( apply ( makeZotFromStr "11010100100" ) $ Int 5 ) $ Int 7

testZot2 = apply ( makeZotFromStr skk ) $ Int 3

testRev = makeZotFromStr ( rev ++ "1101000" )

app :: String -> String -> String
app f a = '1' : f ++ a


skk = ( "101010100" `app` "1010100" ) `app` "1010100"

makeZotFromStr :: String -> Fun
makeZotFromStr = makeZot . strToBits
	where
	strToBits ""			= [ ]
	strToBits ( '0' : rest )	= O : strToBits rest
	strToBits ( '1' : rest )	= I : strToBits rest

makeZot :: [ Bit ] -> Fun
makeZot [ ] = empty
makeZot str = apply ( makeZot $ init str ) $ case last str of
	I	-> one
	O	-> zero

($$) = apply

interrogate :: Fun -> Fun
interrogate f = f $$ i $$ i $$ i $$ k

output = k $$ ( k $$ ( k $$ ( k $$ ( k $$ ( k $$ i ) ) ) ) )

pr :: Fun -> IO ()
pr x = print $ interrogate x $$ Int 0 $$ Int 1

rev = "1111010101001110101010010011010100100100111010101001110101010011010100101010100111010101001101010011010101001101010010101010011101010100111010101001101010010101010011101010100110101001101010100110101001010101001110101010011010100110101010011010100110101010011101010100111010101001110101010011101010100100110101001001101010010011010100100110101001010100111010101001101010011010101001101010011010101001101010010101001110101010011101010100110101001010101001110101010011010100110101010011010100101010100111010101001101010011010101001110101010011010100101010100101010011101010100110101001010100111010101001110101010011010100101010100111010101001101010010101001110101010011010100101010100101010011010100111010101001101010011010101001001010100110101001010100111010101001101010011010101001101010011010101001101010010101001110101010011101010100110101001010101001110101010011010100110101010011010100101010100111010101001101010011010101001110101010011010100101010100101010011101010100110101001010100111010101001110101010011010100101010100111010101001101010010101001110101010011010100101010100101010011010100111010101001101010011010101001001010100110101001010100111010101001101010010101001010100"
