module Main where

import qualified Zot
import qualified LambdaToSki
import qualified SkiToLambda
import qualified SkiToZot
import qualified ZotToSki
import qualified AddEcho

import System.Environment ( getArgs )

main :: IO ()
main = do
	cmd : args <- getArgs
	case cmd of
		"-"		-> Zot.main
		"lambdaToSki"	-> LambdaToSki.main
		"skiToLambda"	-> SkiToLambda.main args
		"skiToZot"	-> SkiToZot.main
		"zotToSki"	-> ZotToSki.main
		"arg"		-> AddEcho.main args
