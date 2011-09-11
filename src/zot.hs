module Main where

import qualified Zot		( main, mainFile )
import qualified SkiToZot	( main )
import qualified ZotToSki	( main )
import qualified LambdaToSki	( main )
import qualified SkiToLambda	( main )

import System.Environment ( getArgs )
import Data.List ( isSuffixOf )

main :: IO ()
main = do
	ca@( ~( cmd : args ) ) <- getArgs
	let command = if null ca then "" else cmd
	case command of
		"skiToZot"				-> SkiToZot.main
		"zotToSki"				-> ZotToSki.main
		"lambdaToSki"				-> LambdaToSki.main
		"skiToLambda"				-> SkiToLambda.main args
		"arg"					-> interact $ (++ concat args )
		"-"					-> Zot.main
		_	| ".zot" `isSuffixOf` command	-> Zot.mainFile command
			| otherwise			-> error "bad arguments"
