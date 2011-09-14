module SkiToLambda ( main ) where

import Data.List ( minimumBy )
import Data.Ord ( comparing )

data Lambda = Var String | Apply Lambda Lambda | Fun String Lambda | I | K

instance Show Lambda where
	show ( Var v )		= v
	show K			= "K"
	show I			= "I"
	show ( Apply f a )	= showLambdaApply f ++ " " ++ addParens show a
	show ( Fun pr ex )	= "\\" ++ pr ++ showLambdaFun ex
		where
		showLambdaFun ( Fun p e )	= " " ++ p ++ showLambdaFun e
		showLambdaFun e			= " -> " ++ showLambdaApply e

showLambdaApply :: Lambda -> String
showLambdaApply ( Apply f a )	= showLambdaApply f ++ " " ++ addParens show a
showLambdaApply e		= addParens show e

addParens :: ( Lambda -> String ) -> Lambda -> String
addParens sh a@( Apply _ _ )	= "(" ++ sh a ++ ")" 
addParens sh f@( Fun _ _ )	= "(" ++ sh f ++ ")"
addParens sh e			= sh e

size :: Lambda -> Int
size ( Apply f a )	= size f + size a
size ( Fun _ e )	= 1 + size e
size _			= 1

main :: [ String ] -> IO ()
main args = do
	let rest = args
	case rest of
		[ ]		-> interact $ ( ++ "\n" ) . show . applyI .
			applyToMin . one . readSKI 0
		[ "-h" ]	-> interact $ unlines . devide 80 . show . toKI .
			applyI .
			applyToMin .
			one . readSKI 0
		_		-> error "bad arguments"
	where
	one ( x, _, _ ) = x

devide :: Int -> [ a ] -> [ [ a ] ]
devide _ [ ]	= [ ]
devide n xs	= take n xs : devide n ( drop n xs )

readSKI :: Int -> String -> ( Lambda, String, Int )
readSKI n ( '`' : rest ) = let
	( f, rest2, n2 ) = readSKI n rest
	( a, rest3, n3 ) = readSKI n2 rest2 in
	( Apply f a, rest3, n3 )
readSKI n ( 'i' : rest ) = ( Fun x $ Var x, rest, n + 1 )
	where	x = 'x' : show n
readSKI n ( 'k' : rest ) = ( Fun x $ Fun y $ Var x, rest, n + 1 )
	where	x = 'x' : show n
		y = 'y' : show n
readSKI n ( 's' : rest ) = ( Fun x $ Fun y $ Fun z $
	Apply ( Apply ( Var x ) ( Var z ) ) ( Apply ( Var y ) ( Var z ) ), rest, n +1 )
	where	x = 'x' : show n
		y = 'y' : show n
		z = 'z' : show n
readSKI _ _		= error "readSKI error"

toKI :: Lambda -> Lambda
toKI ( Fun p ( Var v ) )
	| p == v	= I
toKI ( Fun p1 ( Fun p2 ( Var v ) ) )
	| p1 == v	= K
	| p2 == v	= Apply K I
toKI ( Apply f a )	= Apply ( toKI f ) ( toKI a )
toKI ( Fun p e )	= Fun p $ toKI e
toKI kiv		= kiv

applyToMin :: Lambda -> Lambda
applyToMin e = minimumBy ( comparing size ) $ take 15 $ iterate apply e

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
applyI ki		= ki

apply :: Lambda -> Lambda
apply ( Apply fr ar ) = let
	newLambda = case apply fr of
		Fun p e -> applyPara p ( apply ar ) e
		nf	-> Apply nf ( apply ar ) in
	newLambda
apply ( Fun p e ) = Fun p ( apply e )
apply v@( Var _ ) = v
apply ki	= ki

applyPara :: String -> Lambda -> Lambda -> Lambda
applyPara p a1 ( Var v )
	| p == v		= a1
	| otherwise		= Var v
applyPara p a1 ( Apply f a2 )	= Apply ( applyPara p a1 f ) ( applyPara p a1 a2 )
applyPara p1 a1 ( Fun p2 e )
	| p1 == p2		= Fun p2 e
	| otherwise		= Fun p2 $ applyPara p1 a1 e
applyPara _ _ ki		= ki
