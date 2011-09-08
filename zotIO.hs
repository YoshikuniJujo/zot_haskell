{-# LANGUAGE PackageImports #-}

import "monads-tf" Control.Monad.State

data Fun = Fun { apply_ :: Fun -> IO Fun } | Int Int

apply :: Fun -> Fun -> IO Fun
apply ( Fun f )		= f
apply ( Int int )	= apply i

instance Show Fun where
	show ( Int i )	= show i
	show Fun{ }	= "function"

k, i :: Fun
s = Fun $ \x -> return $ Fun $ \y -> return $ Fun $ \z -> do
	xz <- apply x z
	yz <- apply y z
	apply xz yz
k = Fun $ \x -> return $ Fun $ \y -> return x
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

main :: IO ()
main = do
	fun <- getContents >>= makeZot
	funOut <- apply fun =<< output
	apply funOut pr
	putStrLn ""
	

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

($$) :: Fun -> Fun -> IO Fun
f $$ a = apply f a

makeZot :: String -> IO Fun
makeZot "" = return $ empty
makeZot bs = do
	f <- makeZot $ init bs
	apply f $ case last bs of
		'1' -> one
		'0' -> zero

pr :: Fun
pr = Fun $ \x -> do
	ix <- interrogate x
	ix0 <- apply ix ( Int 0 )
	apply ix0 ( Int 1 ) >>= putStr . show
	return pr

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

rev = "1111010101001110101010010011010100100100111010101001110101010011010100101010100111010101001101010011010101001101010010101010011101010100111010101001101010010101010011101010100110101001101010100110101001010101001110101010011010100110101010011010100110101010011101010100111010101001110101010011101010100100110101001001101010010011010100100110101001010100111010101001101010011010101001101010011010101001101010010101001110101010011101010100110101001010101001110101010011010100110101010011010100101010100111010101001101010011010101001110101010011010100101010100101010011101010100110101001010100111010101001110101010011010100101010100111010101001101010010101001110101010011010100101010100101010011010100111010101001101010011010101001001010100110101001010100111010101001101010011010101001101010011010101001101010010101001110101010011101010100110101001010101001110101010011010100110101010011010100101010100111010101001101010011010101001110101010011010100101010100101010011101010100110101001010100111010101001110101010011010100101010100111010101001101010010101001110101010011010100101010100101010011010100111010101001101010011010101001001010100110101001010100111010101001101010010101001010100"
