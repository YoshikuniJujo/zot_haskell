module Zot ( main, mainFile ) where

import Data.Char

-- data Fun = Fun { apply_ :: Fun -> IO Fun } | Int Int
data Fun = Fun ( Fun -> IO Fun ) | Int Int

apply :: Fun -> Fun -> IO Fun
apply ( Fun f )		= f
apply ( Int _ )		= apply i

instance Show Fun where
	show ( Int int )	= show int
	show Fun{ }		= "function"

s, k, i :: Fun
s = Fun $ \x -> return $ Fun $ \y -> return $ Fun $ \z -> do
	xz <- apply x z
	yz <- apply y z
	apply xz yz
k = Fun $ \x -> return $ Fun $ \_ -> return x
i = Fun return

empty, zero, one :: Fun
empty = Fun $ \c -> apply c i
zero = Fun $ \c -> apply c $ Fun $ \f -> do
	fs <- apply f s
	apply fs k
one = Fun $ \c -> return $ Fun $ \ll -> apply ll $
	Fun $ \l -> return $ Fun $ \rr -> apply rr $ Fun $ \r -> do
		lr <- apply l r
		apply c lr

remCom :: String -> String
remCom = unlines . map ( takeWhile ( /= '#' ) ) . lines

main :: IO ()
main = do
	fun <- getContents >>= makeZot . filter ( not . isSpace ) . remCom
	funOut <- apply fun =<< output
	_ <- apply funOut pr
--	apply fun pr
	putStrLn ""

mainFile :: String -> IO ()
mainFile fn = do
	prg <- readFile fn
	arg <- getContents
	fun <- makeZot $ filter ( not . isSpace ) $ remCom $ prg ++ arg
	funOut <- apply fun =<< output
	_ <- apply funOut pr
	putStrLn ""

{-
main_ :: IO ()
main_ = do
	apply i ( Int 3 ) >>= print
	ii <- makeZot "100"
	apply ii ( Int 3 ) >>= print
	kkii <- makeZot "10100"
	kkii4 <- apply kkii ( Int 4 )
	print kkii4
	apply kkii4 ( Int 7 ) >>= print
	k8 <- apply k ( Int 8 )
	apply k8 ( Int 3 ) >>= print
	ki <- apply k i
	ki3 <- apply ki ( Int 3 )
	apply ki3 ( Int 5 ) >>= print
	intOne <- interrogate =<< output
	intOne0 <- apply intOne ( Int 0 )
	intOne01 <- apply intOne0 ( Int 1 )
	apply intOne01 ( Int 2 ) >>= print
	pr1011
	putStrLn ""
	apply lst10 pr
	putStrLn ""
	rv <- makeZot $ rev ++ "1101000"
	rvo <- apply rv =<< output
	apply rvo pr
	putStrLn ""
	lst10f <- lst10''
	apply lst10f pr
	putStrLn ""
	lst10f <- lst10'
--	lst10fo <- apply lst10f =<< output
	apply lst10f pr
	putStrLn ""
	kif <- ioki
	kif3 <- apply kif ( Int 3 )
	apply kif3 ( Int 5 ) >>= print

($$) :: Fun -> Fun -> IO Fun
f $$ a = apply f a
-}

makeZot :: String -> IO Fun
makeZot "" = return empty
makeZot bs = do
	f <- makeZot $ init bs
	apply f $ case last bs of
		'1'	-> one
		'0'	-> zero
		_	-> error "makeZot: appear not '0' or '1'"

pr :: Fun
pr = Fun $ \x -> do
	ix <- interrogate x
	ix0 <- apply ix ( Int 0 )
	apply ix0 ( Int 1 ) >>= putStr . show
	return pr

{-
pr1011 :: IO Fun
pr1011 = do
	pr2 <- apply pr one
	pr3 <- apply pr2 zero
	pr4 <- apply pr3 one
	apply pr4 one

lst10 :: Fun
lst10 = Fun $ \f -> do
--	( \g -> g zero )
	f1 <- apply f one
	apply f1 zero

ioki :: IO Fun
ioki = makeZot "110100100"

iok1 :: IO Fun
iok1 = makeZot "1101001"

lst10' :: IO Fun
lst10' = makeZot $ skiToZot "``s``si`k0`k1"
-- lst10' = makeZot $ skiToZot "``si`k1"
-- lst10' = makeZot "1 1 101010100 1 1 101010100 100 1 1010100 1 1 1010100 0"
-- lst10' = makeZot "11101010100100110101001"

skiToZot :: String -> String
skiToZot ""		= ""
skiToZot ( '`' : rest )	= '1' : skiToZot rest
skiToZot ( 's' : rest ) = "101010100" ++ skiToZot rest
skiToZot ( 'k' : rest ) = "1010100" ++ skiToZot rest
skiToZot ( 'i' : rest ) = "100" ++ skiToZot rest
skiToZot ( '1' : rest ) = "1" ++ skiToZot rest
skiToZot ( '0' : rest ) = "0" ++ skiToZot rest

lst10'' :: IO Fun
lst10'' = do
	si <- apply s i
	k1 <- apply k one
	k0 <- apply k zero
	sik1 <- apply si k1
	ssik1 <- apply s sik1
	apply ssik1 k0
-}

interrogate :: Fun -> IO Fun
interrogate f = do
	fi <- apply f i
	fii <- apply fi i
	fiii <- apply fii i
	apply fiii k

output :: IO Fun
output = do
	ki <- apply k i
	kki <- apply k ki
	kkki <- apply k kki
	kkkki <- apply k kkki
	kkkkki <- apply k kkkki
	apply k kkkkki

-- rev = "1111010101001110101010010011010100100100111010101001110101010011010100101010100111010101001101010011010101001101010010101010011101010100111010101001101010010101010011101010100110101001101010100110101001010101001110101010011010100110101010011010100110101010011101010100111010101001110101010011101010100100110101001001101010010011010100100110101001010100111010101001101010011010101001101010011010101001101010010101001110101010011101010100110101001010101001110101010011010100110101010011010100101010100111010101001101010011010101001110101010011010100101010100101010011101010100110101001010100111010101001110101010011010100101010100111010101001101010010101001110101010011010100101010100101010011010100111010101001101010011010101001001010100110101001010100111010101001101010011010101001101010011010101001101010010101001110101010011101010100110101001010101001110101010011010100110101010011010100101010100111010101001101010011010101001110101010011010100101010100101010011101010100110101001010100111010101001110101010011010100101010100111010101001101010010101001110101010011010100101010100101010011010100111010101001101010011010101001001010100110101001010100111010101001101010010101001010100"
