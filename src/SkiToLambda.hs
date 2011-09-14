module SkiToLambda ( main ) where

import Data.List ( minimumBy )
import Data.Ord ( comparing )
import Control.Arrow ( ( &&& ) )

infixl 9 :$:
infixr 8 :->
data Lambda = Var String | Lambda :$: Lambda | String :-> Lambda | K | I

instance Show Lambda where
	show ( Var v )		= v
	show ap@( _ :$: _ )	= showAp ap
		where
		showAp ( f :$: a )	= showAp f ++ " " ++ par show a
		showAp e		= par show e
		par sh a@( _ :$: _ )	= "(" ++ sh a ++ ")" 
		par sh f@( _ :-> _ )	= "(" ++ sh f ++ ")"
		par sh e		= sh e
	show f@( _ :-> _ )		= '\\' : showFun f
		where
		showFun ( p :-> e )	= p ++ " " ++ showFun e
		showFun e		= "-> " ++ show e
	show K			= "K"
	show I			= "I"

size :: Lambda -> Int
size ( f :$: a )	= size f + size a
size ( _ :-> e )	= 1 + size e
size _			= 1

main :: [ String ] -> IO ()
main args = interact $ case args of
	[ ]		-> ( ++ "\n" ) . show . skiToLambda
	[ "-h" ]	-> unlines . devide 80 . show . ki . skiToLambda
	_		-> error "bad arguments"
	where
	devide _ [ ]	= [ ]
	devide n xs	= take n xs : devide n ( drop n xs )
	ki ( p :-> Var v )		| p == v	= I
	ki ( p1 :-> p2 :-> Var v )	| p1 == v	= K
					| p2 == v	= K :$: I
	ki ( f :$: a )					= ki f :$: ki a
	ki ( p :-> e )					= p :-> ki e
	ki kiv						= kiv

skiToLambda :: String -> Lambda
skiToLambda = betaKI .
	minimumBy ( comparing size ) . take 15 . iterate beta . fst . readSki 0

readSki :: Int -> String -> ( Lambda, ( Int, String ) )
readSki n ( '`' : cs )	= let	( f, ncs )	= readSki n cs
				( a, ncs' )	= uncurry readSki ncs in
				( f :$: a, ncs' )
readSki n ( c : cs )	= ( case c of
	'i'	-> fx x
	'k'	-> fx $ fy x
	's'	-> fx $ fy $ fz $ x :$: z :$: ( y :$: z )
	_	-> error "readSki error", ( n + 1, cs ) ) 
	where
	[ ( fx, x ), ( fy, y ), ( fz, z ) ] =
		( ( ( :-> ) &&& Var ) . ( : show n ) ) `map` "xyz"
readSki _ _		= error "readSki error"

beta, betaKI :: Lambda -> Lambda
beta ( fr :$: ar )	= case beta fr of
				p :-> e	-> applyPara p ( beta ar ) e
				nf	-> nf :$: beta ar
beta ( p :-> e )	= p :-> beta e
beta kiv		= kiv

betaKI ( ( p :-> v@( Var x ) ) :$: ar )
	| p == x	= ar
	| otherwise	= v
betaKI ( ( p :-> f@( q :-> Var x ) ) :$: a )
	| q == x	= f
	| p == x	= "_" :-> a
betaKI ( f :$: a )	= betaKI f :$: betaKI a
betaKI ( p :-> e )	= p :-> betaKI e
betaKI kiv		= kiv

applyPara :: String -> Lambda -> Lambda -> Lambda
applyPara p a1 ( Var v )
	| p == v		= a1
	| otherwise		= Var v
applyPara p a1 ( f :$: a2 )	= applyPara p a1 f :$: applyPara p a1 a2
applyPara p1 a1 ( p2 :-> e )
	| p1 == p2		= p2 :-> e
	| otherwise		= p2 :-> applyPara p1 a1 e
applyPara _ _ ki		= ki
