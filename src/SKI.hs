module SKI where

data Rec a
	= In { out :: Rec a -> Rec a }
	| Int { fromInt :: Int }
	| Char Char | Bool Bool
	| Error String

instance Eq ( Rec a ) where
	Int i == Int j		= i == j
	Char c == Char d	= c == d
	Bool b == Bool c	= b == c
	In _ == In _		= error "can not compare"
	_ == _			= False

instance Show ( Rec a ) where
	show ( Int i )		= show i
	show ( Error e )	= "Error: " ++ e
	show _			= "function"

suc :: Rec a
suc = In scc
	where
	scc ( Int i )	= Int $ succ i
	scc ( Char c )	= Char $ succ c
	scc _		= error "not int"

out_ :: Rec a -> Rec a -> Rec a
out_ x = case x of
	In f	-> f
	_	-> \_ -> Error "not function"

oi :: Rec a -> Rec a
oi x = x
ii :: Rec a
ii = In oi

ok :: Rec a -> Rec a
ok x = In $ \_ -> x
k :: Rec a
k = In ok

os :: Rec a -> Rec a
-- s x y z = x z ( y z )
os x = In $ \y -> In $ \z -> ( out_ $ out_ x z ) ( out_ y z )
s :: Rec a
s = In os

readChurch :: Rec a -> Rec a
readChurch cn = cn `out_` suc `out_` ( Int 0 )

mkski :: String -> Rec a
mkski = fst . makeSKI

makeSKI :: String -> ( Rec a, String )
makeSKI ( '`' : rest )	= let
	( c, rest' )	= makeSKI rest
	( c', rest'' )	= makeSKI rest' in case c of
		In f	-> ( f c', rest'' )
		_	-> ( Error "in makeSKI", rest'' )
--	( out c c', rest'' )
makeSKI ( 's' : rest )	= ( s, rest )
makeSKI ( 'k' : rest )	= ( k, rest )
makeSKI ( 'i' : rest )	= ( ii, rest )
makeSKI _		= error "makeSKI error"

sc :: Rec a
sc = mkski "`s``s`ks``s`kki"

mul :: Rec a -> Rec a -> Rec a
mul x y = out ( out x ( out y sc ) ) ( out k ii )
