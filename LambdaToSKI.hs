module LambdaToSKI where

import Parse
import Data.Char

-- data SKI = S | K | I deriving Show

data Lambda = Var String | Apply Lambda Lambda | Fun String Lambda
	| S | K | I
	deriving Show

churchTwo :: Lambda
churchTwo =
	Fun "f" $ Fun "x" $ Apply ( Var "f" ) $ Apply ( Var "f" ) ( Var "x" )

lambdaToSKI :: Lambda -> Lambda
lambdaToSKI ( Fun x e ) = out x e
lambdaToSKI ( Apply f a ) = Apply ( lambdaToSKI f ) ( lambdaToSKI a )
lambdaToSKI nf = error $ "lambdaToSKI error: " ++ show nf

notHave :: String -> Lambda -> Bool
notHave x0 ( Var x1 )
	| x0 == x1		= False
	| otherwise		= True
notHave x0 ( Fun _ e )		= notHave x0 e
notHave x0 ( Apply f a )	= notHave x0 f && notHave x0 a
notHave _ _			= True

out :: String -> Lambda -> Lambda
out x0 ( Var x1 )
	| x1 == x0	= I
	| otherwise	= Apply K $ Var x1
out x0 ( Apply f ( Var x1 ) )
	| x1 == x0 && notHave x0 f	= f
out x0 ( Apply f a )	= Apply ( Apply S  $ out x0 f ) $ out x0 a
out x0 ( Fun x1 e )
	| x0 == x1	= Apply K $ out x0 e
	| otherwise	= ( out x0 ) $ out x1 e
out x0 ski		= Apply K $ ski

showApply :: Lambda -> String
showApply S = "s"
showApply K = "k"
showApply I = "i"
showApply ( Apply f a ) = "`" ++ showApply f ++ showApply a

showSKI = showApply . lambdaToSKI

readLambda :: String -> Lambda
readLambda = fst . fst . head . ( parseLambda >*> eof ) . lexer

parseLambda :: Parse String Lambda
parseLambda = parseApply `alt` parseFun

parseAtom :: Parse String Lambda
parseAtom = ( spot ( isLower . head ) `build` Var ) `alt`
	parseParens

parseParens :: Parse String Lambda
parseParens = ( token "(" >*> parseLambda >*> token ")" ) `build`
	( \( "(", ( body, ")" ) ) -> body )

parseApply :: Parse String Lambda
parseApply = recL1 Apply parseAtom

parseFun :: Parse String Lambda
parseFun =
	( token "\\" >*> parsePars >*> token "->" >*> parseLambda )
		`build` ( \( "\\", ( vs, ( "->", e ) ) ) -> makeFun vs e )

makeFun :: [ String ] -> Lambda -> Lambda
makeFun [ p ] ex	= Fun p ex
makeFun ( p : ps ) ex	= Fun p $ makeFun ps ex

parsePars :: Parse String [ String ]
parsePars = ( spot ( isLower . head ) `build` (:[]) ) `alt`
	( ( spot ( isLower . head ) >*> parsePars ) `build`
		( \( x, xs ) -> x : xs ) )

lexer :: String -> [ String ]
lexer "\n"			= [ ]
lexer ""			= [ ]
lexer ( '\\' : rest )		= "\\" : lexer rest
lexer ( '-' : '>' : rest )	= "->" : lexer rest
lexer ( ' ' : rest )		= lexer rest
lexer ( '\n' : rest )		= lexer rest
lexer ( '(' : rest )		= "(" : lexer rest
lexer ( ')' : rest )		= ")" : lexer rest
lexer ca@( c : _ )
	| isLower c		= let ( ret, rest ) = span isAlphaNum ca in
					ret : lexer rest
lexer ca			= error $ "lexer error: " ++ ca
