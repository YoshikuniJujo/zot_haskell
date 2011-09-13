module ZotToSki ( main ) where

import Parse ( Parse, token, tokens, ( >*> ), alt, build, eof )

main :: IO ()
main = interact $ zotToSKI . concat . lines

zotToSKI :: String -> String
zotToSKI = fst . head . ( parseZot >*> eof `build` fst )

parseZot :: Parse Char String
parseZot = parseApply `alt` parseS `alt` parseK `alt` parseI

parseApply :: Parse Char String
parseApply = token '1' >*> parseZot >*> parseZot `build`
	\( _, ( ski1, ski2 ) ) -> '`' : ski1 ++ ski2

parseS, parseK, parseI :: Parse Char String
parseS = tokens "101010100" `build` const "s"
parseK = tokens "1010100" `build` const "k"
parseI = tokens "100" `build` const "i"
