module Parse (
	Parse,
	none,
	succeed,
	spot,
	token,
	tokens,
	eof,
	(>*>),
	alt,
	build,
	list1,
	recL1
) where

import Control.Arrow ( ( &&& ) )

infixr	8 >*>
infix	7 `build`
infixl	6 `alt`

type Parse a b = [ a ] -> [ ( b, [ a ] ) ]

none :: Parse a b
none = const [ ]

succeed :: b -> Parse a b
succeed = curry ( : [] )

spot :: ( a -> Bool ) -> Parse a a
spot p ( x : xs )	| p x	= [ ( x, xs ) ]
spot _ _			= [ ]

token :: Eq a => a -> Parse a a
token = spot . ( == )

tokens :: Eq a => [ a ] -> Parse a [ a ]
tokens [ ]		= succeed [ ]
tokens ( x : xs )	= token x >*> tokens xs `build` uncurry (:)

(>*>) :: Parse a b -> Parse a c -> Parse a ( b, c )
( p1 >*> p2 ) inp =
	[ ( ( y, z ), rem2 ) | ( y, rem1 ) <- p1 inp, ( z, rem2 ) <- p2 rem1 ]

alt :: Parse a b -> Parse a b -> Parse a b
p1 `alt` p2 = uncurry ( ++ ) . ( p1 &&& p2 )

build :: Parse a b -> ( b -> c ) -> Parse a c
build p f inp = [ ( f x, rem1 ) | ( x, rem1 ) <- p inp ]

recL1 :: ( b -> b -> b ) -> Parse a b -> Parse a b
recL1 f p = ( p >*> recL1' f p ) `build` ( \( x, xs ) -> xs x )

recL1' :: ( b -> c -> b ) -> Parse a c -> Parse a ( b -> b )
recL1' f p = succeed id `alt`
	( ( p >*> recL1' f p ) `build` ( \( x, xs ) y -> xs ( f y x ) ) )

eof :: Parse a ()
eof [ ]	= [ ( (), [ ] ) ]
eof _	= [ ]

list1 :: Parse a b -> Parse a [ b ]
list1 p = p `build` ( : [] ) `alt` p >*> list1 p `build` uncurry ( : )
