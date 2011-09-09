
main :: IO ()
main = interact skiToLambda

skiToLambda :: String -> String
skiToLambda = fst . parens

parens :: String -> ( String, String )
parens ( '`' : rest ) =	let
	( f, rest2 ) = parens rest
	( a, rest3 ) = parens rest2 in
	( "(" ++ f ++ " " ++ a ++ ")", rest3 )
parens ( c : rest ) = ( [ c ], rest )
