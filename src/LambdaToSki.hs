module LambdaToSki ( main ) where

import Parse ( Parse, spot, token, eof, ( >*> ), alt, build, list1, recL1 )
import Data.Char ( isLower, isAlphaNum )

main :: IO ()
main = interact $ show . lambdaToSki . readLambda

data Lambda = Var String | Apply Lambda Lambda | Fun String Lambda | S | K | I

instance Show Lambda where
	show S			= "s"
	show K			= "k"
	show I			= "i"
	show ( Apply f a )	= "`" ++ show f ++ show a
	show ( Fun p e )	= "(fun " ++ p ++ " " ++ show e ++ ")"
	show ( Var v )		= "(var " ++ v ++ ")"

onlySKI :: Lambda -> Bool
onlySKI S		= True
onlySKI K		= True
onlySKI I		= True
onlySKI ( Apply f a )	= onlySKI f && onlySKI a
onlySKI _		= False

makeFun :: [ String ] -> Lambda -> Lambda
makeFun [ p ] ex	= Fun p ex
makeFun ( p : ps ) ex	= Fun p $ makeFun ps ex
makeFun _ _		= error "makeFun error: need 1 parameter at least"

lambdaToSki :: Lambda -> Lambda
lambdaToSki ( Fun x e )		= out x e
lambdaToSki ( Apply f a )	= Apply ( lambdaToSki f ) ( lambdaToSki a )
lambdaToSki _			= error $ "lambdaToSki error"

out :: String -> Lambda -> Lambda
out _ e
	| onlySKI e	= Apply K e
out x0 v@( Var x1 )
	| x1 == x0	= I
	| otherwise	= Apply K v
out x0 ( Apply f a )	= Apply ( Apply S  $ out x0 f ) $ out x0 a
out x0 ( Fun x1 e )
	| x0 == x1	= Apply K $ out x0 e
	| otherwise	= out x0 $ out x1 e
out _ _			= error "never occur"

readLambda :: String -> Lambda
readLambda = fst . fst . head . ( parseLambda >*> eof ) . lexer

parseLambda :: Parse String Lambda
parseLambda = parseApply `alt` parseFun

parseApply :: Parse String Lambda
parseApply = recL1 Apply parseAtom

parseFun :: Parse String Lambda
parseFun = token "\\" >*> parseParams >*> token "->" >*> parseLambda
	`build` ( \( "\\", ( ps, ( "->", e ) ) ) -> makeFun ps e )

parseAtom :: Parse String Lambda
parseAtom = ( spot ( isLower . head ) `build` Var ) `alt` parseParens

parseParens :: Parse String Lambda
parseParens = token "(" >*> parseLambda >*> token ")" `build` fst . snd

parseParams :: Parse String [ String ]
parseParams = list1 $ spot $ isLower . head

lexer :: String -> [ String ]
lexer ""			= [ ]
lexer ( ' ' : rest )		= lexer rest
lexer ( '\n' : rest )		= lexer rest
lexer ( '\\' : rest )		= "\\" : lexer rest
lexer ( '-' : '>' : rest )	= "->" : lexer rest
lexer ( '(' : rest )		= "(" : lexer rest
lexer ( ')' : rest )		= ")" : lexer rest
lexer ca@( c : _ )
	| isLower c		= let ( ret, rest ) = span isAlphaNum ca in
					ret : lexer rest
lexer ca			= error $ "lexer error: " ++ ca
