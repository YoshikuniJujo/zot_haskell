module LambdaToSki ( main ) where

-- import SKI ( mkski, Rec( .. ), fromInt, out, readChurch )
import ReadLambda ( readLambda, showSKI )

main :: IO ()
main = interact lambdaToSKI

{-
fromLambda :: String -> Rec a
fromLambda = mkski . showSKI . readLambda
-}

lambdaToSKI :: String -> String
lambdaToSKI = showSKI . readLambda

{-
mulString = "\\x y -> x ( y ( \\g f xx -> f ( g f xx ) ) ) ( \\f x -> x )"

mul :: Rec a -> Rec a -> Rec a
mul x y = out ( out ( fromLambda mulString ) x ) y

zero :: Rec a
zero = fromLambda "\\f x -> x"

suc :: Rec a -> Rec a
suc = out ( fromLambda "\\g f x -> f ( g f x )" )

two = suc ( suc zero )
three = suc two

mul', zero', suc', two' :: String
mul' = lambdaToSKI mulString
zero' = lambdaToSKI "\\f x -> x"
suc' = lambdaToSKI "\\g f x -> f ( g f x)"
two' = apply suc' $ apply suc' zero'
three' = apply suc' two'

apply :: String -> String -> String
apply f a = '`' : f ++ a

cons, car, cdr :: String
cons = "\\s b f -> f s b"
car = "\\p -> p ( \\x y -> x )"
cdr = "\\p -> p ( \\x y -> y )"

true, false :: String
true = "\\x y -> x"
false = "\\x y -> y"

getBool :: Rec a -> Bool
getBool x = let Bool b = out ( out x ( Bool True ) ) ( Bool False ) in b

getIntList :: Rec a -> [ Int ]
getIntList lst
--	| out ( out lst ( Bool True ) ) ( Bool False ) == Bool False	= [ ]
	| isFalse lst							= [ ]
	| otherwise							=
		fromInt ( readChurch ( out lst ( fromLambda true ) ) ) :
		getIntList ( out lst ( fromLambda false ) )

isFalse :: Rec a -> Bool
isFalse ( In x )	= case x ( Bool True ) of
	In y	-> y ( Bool False ) == Bool False
	_	-> False
isFalse _		= False

someList :: Rec a
someList = mkski $ apply ( apply ( lambdaToSKI cons ) two' ) ( lambdaToSKI false )

makeIntList :: [ Int ] -> String
makeIntList [ ]		= lambdaToSKI false
makeIntList ( i : is )	=
	apply ( apply ( lambdaToSKI cons ) $ makeChurch i ) $ makeIntList is

makeChurch :: Int -> String
makeChurch 0 = "`ki"
makeChurch i = apply succ' $ makeChurch ( i - 1 )

succ' = "`s``s`ks``s`kki"

-}
