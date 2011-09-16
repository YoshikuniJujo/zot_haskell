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

import Control.Arrow ( first, ( &&& ) )

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

eof :: Parse a ()
eof [ ]	= [ ( (), [ ] ) ]
eof _	= [ ]

(>*>) :: Parse a b -> Parse a c -> Parse a ( b, c )
( p >*> q ) inp = [ ( ( x, y ), r' ) | ( x, r ) <- p inp, ( y, r' ) <- q r ]

alt :: Parse a b -> Parse a b -> Parse a b
p1 `alt` p2 = uncurry ( ++ ) . ( p1 &&& p2 )

build :: Parse a b -> ( b -> c ) -> Parse a c
build p f = map ( first f ) . p

list1 :: Parse a b -> Parse a [ b ]
list1 p = p `build` ( : [] ) `alt` p >*> list1 p `build` uncurry ( : )

recL1 :: ( b -> b -> b ) -> Parse a b -> Parse a b
recL1 f p = ( p >*> recL1' f p ) `build` ( \( x, xs ) -> xs x )

recL1' :: ( b -> c -> b ) -> Parse a c -> Parse a ( b -> b )
recL1' f p = succeed id `alt`
	( p >*> recL1' f p `build` ( \( x, xs ) y -> xs ( f y x ) ) )
