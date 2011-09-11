module SkiToLambda ( main ) where

data Lambda = Var String | Apply Lambda Lambda | Fun String Lambda
	deriving Show

main :: [ String ] -> IO ()
main args = do
--	( n : rest ) <- getArgs
	let ( n : rest ) = args
	case rest of
		[ ]		-> interact $ ( ++ "\n" ) . showLambda . applyApp . applyI .
			times ( read n ) apply . one . readSKI 0
		[ "-h" ]	-> interact $ unlines . devide 80 . showLambdaTop . applyApp . applyI .
			times ( read n ) apply . one . readSKI 0
		_		-> error "bad arguments"
	where
	one ( x, _, _ ) = x

times :: Int -> ( a -> a ) -> a -> a
times 0 _ x = x
times n f x = times ( n - 1 ) f $ f x

devide :: Int -> [ a ] -> [ [ a ] ]
devide _ [ ]	= [ ]
devide n xs	= take n xs : devide n ( drop n xs )

{-
skiToLambda :: String -> String
skiToLambda = fst . parens
-}

readSKI :: Int -> String -> ( Lambda, String, Int )
readSKI n ( '`' : rest ) = let
	( f, rest2, n2 ) = readSKI n rest
	( a, rest3, n3 ) = readSKI n2 rest2 in
	( Apply f a, rest3, n3 )
readSKI n ( 'i' : rest ) = ( Fun x $ Var x, rest, n + 1 )
	where	x = "x" ++ show n
readSKI n ( 'k' : rest ) = ( Fun x $ Fun y $ Var x, rest, n + 1 )
	where	x = "x" ++ show n
		y = "y" ++ show n
readSKI n ( 's' : rest ) = ( Fun x $ Fun y $ Fun z $
	Apply ( Apply ( Var x ) ( Var z ) ) ( Apply ( Var y ) ( Var z ) ), rest, n +1 )
	where	x = "x" ++ show n
		y = "y" ++ show n
		z = "z" ++ show n
readSKI _ _		= error "readSKI error"

showLambda, showLambdaH, showLambdaApply, showLambdaFun, showLambdaTop :: Lambda -> String
showLambda ( Var v ) = v
showLambda ( Apply f a ) = "(" ++ showLambda f ++ " " ++ showLambda a ++ ")"
showLambda ( Fun p e ) = "(\\" ++ p ++ " -> " ++ showLambda e ++ ")"

showLambdaTop ( Apply f a )	= showLambdaApply f ++ " " ++ showLambdaH a
showLambdaTop ( Fun p e )	= "\\" ++ p ++ showLambdaFun e
showLambdaTop v			= showLambdaH v

showLambdaH ( Var v ) = v
showLambdaH ( Apply f a ) = "(" ++ showLambdaApply f ++ " " ++ showLambdaH a ++ ")"
showLambdaH ( Fun p ( Var v ) )
	| p == v	= "I"
showLambdaH ( Fun p1 ( Fun p2 ( Var v ) ) )
	| p1 == v	= "K"
	| p2 == v	= "KI"
--	| otherwise	= "hoge"
showLambdaH ( Fun p e ) = "(\\" ++ p ++ showLambdaFun e ++ ")"

showLambdaApply ( Apply f a )	= showLambdaApply f ++ " " ++ showLambdaH a
-- showLambdaApply ( Fun p e )	= "\\" ++ p ++ showLambdaFun e
showLambdaApply e		= showLambdaH e

showLambdaFun ( Fun p e )	= " " ++ p ++ showLambdaFun e
showLambdaFun e			= " -> " ++ showLambdaApply e

{-
parens :: String -> ( String, String )
parens ( '`' : rest ) =	let
	( f, rest2 ) = parens rest
	( a, rest3 ) = parens rest2 in
	( "(" ++ f ++ " " ++ a ++ ")", rest3 )
parens ( c : rest ) = ( [ c ], rest )
-}

applyI :: Lambda -> Lambda
applyI ( Apply ( Fun p ( Var v ) ) ar )
	| p == v	= ar
	| otherwise	= Var v
applyI ( Apply ( Fun p1 ( Fun p2 ( Var v ) ) ) a )
	| p2 == v	= Fun p2 ( Var v )
	| p1 == v	= Fun p2 a
applyI ( Apply f a )	= Apply ( applyI f ) $ applyI a
applyI ( Fun p e )	= Fun p $ applyI e
applyI ( Var v )	= Var v

applyApp :: Lambda -> Lambda
applyApp ( Apply ( Fun p ( Apply ( Var v ) a ) ) f )
	| p == v	= Apply f a
applyApp ( Apply f a )	= Apply ( applyApp f ) $ applyApp a
applyApp ( Fun p e )	= Fun p $ applyApp e
applyApp ( Var v )	= Var v

apply :: Lambda -> Lambda
apply ( Apply fr ar ) =	case apply fr of
	Fun p e -> applyPara p ( apply ar ) e
	nf	-> Apply nf ( apply ar )
apply ( Fun p e ) = Fun p ( apply e )
apply ( Var v ) = Var v

applyPara :: String -> Lambda -> Lambda -> Lambda
applyPara p a1 ( Var v )
	| p == v		= a1
	| otherwise		= Var v
applyPara p a1 ( Apply f a2 )	= Apply ( applyPara p a1 f ) ( applyPara p a1 a2 )
applyPara p1 a1 ( Fun p2 e )
	| p1 == p2		= Fun p2 e
	| otherwise		= Fun p2 $ applyPara p1 a1 e
