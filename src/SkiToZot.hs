module SkiToZot ( main ) where

main :: IO ()
main = interact skiToZot

skiToZot :: String -> String
skiToZot "\n"		= ""
skiToZot ""		= ""
skiToZot ( '`' : rest )	= '1' : skiToZot rest
skiToZot ( 'i' : rest ) = "100" ++ skiToZot rest
skiToZot ( 'k' : rest ) = "1010100" ++ skiToZot rest
skiToZot ( 's' : rest ) = "101010100" ++ skiToZot rest
skiToZot _		= error "skiToZot error"
