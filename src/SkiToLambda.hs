module SkiToLambda ( main ) where

import Data.List ( minimumBy )
import Data.Ord ( comparing )

data Lambda = Var String | Apply Lambda Lambda | Fun String Lambda | I | K

instance Show Lambda where
	show ( Var v )		= v
	show K			= "K"
	show I			= "I"
	show ap@( Apply _ _ )	= showAp ap
		where
		showAp ( Apply f a )	= showAp f ++ " " ++ par show a
		showAp e		= par show e
		par sh a@( Apply _ _ )	= "(" ++ sh a ++ ")" 
		par sh f@( Fun _ _ )	= "(" ++ sh f ++ ")"
		par sh e		= sh e
	show f@( Fun _ _ )	= '\\' : showFun f
		where
		showFun ( Fun p e )	= p ++ " " ++ showFun e
		showFun e		= "-> " ++ show e

size :: Lambda -> Int
size ( Apply f a )	= size f + size a
size ( Fun _ e )	= 1 + size e
size _			= 1

main :: [ String ] -> IO ()
main args = interact $ case args of
	[ ]		-> ( ++ "\n" ) . show . skiToLambda
	[ "-h" ]	-> unlines . devide 80 . show . toKI . skiToLambda
	_		-> error "bad arguments"
	where
	devide _ [ ]	= [ ]
	devide n xs	= take n xs : devide n ( drop n xs )

skiToLambda :: String -> Lambda
skiToLambda = applyI . applyToMin . ( \ ( x, _, _ ) -> x ) . readSKI 0

readSKI :: Int -> String -> ( Lambda, String, Int )
readSKI n ( '`' : rest ) = let	( f, rest2, n2 ) = readSKI n rest
				( a, rest3, n3 ) = readSKI n2 rest2 in
				( Apply f a, rest3, n3 )
readSKI n ( 'i' : rest ) = ( Fun x $ Var x, rest, n + 1 )
	where x = 'x' : show n
readSKI n ( 'k' : rest ) = ( Fun x $ Fun y $ Var x, rest, n + 1 )
	where [ x, y ] = ( : show n ) `map` "xy"
readSKI n ( 's' : rest ) = ( ret, rest, n + 1 )
	where
	ret		= fx $ fy $ fz $ Apply vx vz `Apply` Apply vy vz
	[ fx, fy, fz ]	= Fun `map` [ x, y, z ]
	[ vx, vy, vz ]	= Var `map` [ x, y, z ]
	[ x, y, z ]	= ( : show n ) `map` "xyz"
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

apply :: Lambda -> Lambda
apply ( Apply fr ar ) = let
	newLambda = case apply fr of
		Fun p e -> applyPara p ( apply ar ) e
		nf	-> Apply nf ( apply ar ) in
	newLambda
apply ( Fun p e ) = Fun p ( apply e )
apply v@( Var _ ) = v
apply ki	= ki

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

applyPara :: String -> Lambda -> Lambda -> Lambda
applyPara p a1 ( Var v )
	| p == v		= a1
	| otherwise		= Var v
applyPara p a1 ( Apply f a2 )	= Apply ( applyPara p a1 f ) ( applyPara p a1 a2 )
applyPara p1 a1 ( Fun p2 e )
	| p1 == p2		= Fun p2 e
	| otherwise		= Fun p2 $ applyPara p1 a1 e
applyPara _ _ ki		= ki
