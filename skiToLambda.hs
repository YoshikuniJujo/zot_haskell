import System.Environment

data Lambda = Var String | Apply Lambda Lambda | Fun String Lambda
	deriving Show

main :: IO ()
main = do
	[ n ] <- getArgs
	interact $ ( ++ "\n" ) . showLambda . times ( read n ) apply . one . readSKI 0 -- skiToLambda
	where
	one ( x, _, _ ) = x

times :: Int -> ( a -> a ) -> a -> a
times 0 _ x = x
times n f x = times ( n - 1 ) f $ f x

skiToLambda :: String -> String
skiToLambda = fst . parens

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

showLambda :: Lambda -> String
showLambda ( Var v ) = v
showLambda ( Apply f a ) = "(" ++ showLambda f ++ " " ++ showLambda a ++ ")"
showLambda ( Fun p e ) = "(\\" ++ p ++ " -> " ++ showLambda e ++ ")"

parens :: String -> ( String, String )
parens ( '`' : rest ) =	let
	( f, rest2 ) = parens rest
	( a, rest3 ) = parens rest2 in
	( "(" ++ f ++ " " ++ a ++ ")", rest3 )
parens ( c : rest ) = ( [ c ], rest )

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
