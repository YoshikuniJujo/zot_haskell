module Zot ( main, mainFile ) where

import Data.Char ( isSpace )

data Fun = Fun ( Fun -> IO Fun ) | Int Int

instance Show Fun where
	show ( Int int )	= show int
	show Fun{ }		= "function"

apply :: Fun -> Fun -> IO Fun
apply ( Fun f )		= f
apply ( Int _ )		= apply i

($$) :: Fun -> Fun -> IO Fun
f $$ a = apply f a

infixl 7 $$$

($$$) :: IO Fun -> Fun -> IO Fun
f $$$ a = f >>= ( $$ a )

infixr 8 $$$$

($$$$) :: Fun -> IO Fun -> IO Fun
f $$$$ a = a >>= ( f $$  )

cont :: Fun -> Fun
cont = Fun . flip ( $$ )

retCont :: Fun -> IO Fun
retCont = return . Fun . flip ( $$ )

s, k, i :: Fun
s = Fun $ \x -> return $ Fun $ \y -> return $ Fun $ \z -> do
	xz <- x $$ z
	yz <- y $$ z
	xz $$ yz
k = Fun $ return . Fun . const . return
i = Fun return

empty, zero, one :: Fun
empty	= cont i
zero	= cont $ Fun $ \f -> f $$ s $$$ k
one	= Fun $ \c -> retCont $	Fun $ \l -> retCont $ Fun $ \r -> c $$$$ ( l $$ r )

remCom :: String -> String
remCom = unlines . map ( takeWhile ( /= '#' ) ) . lines

main :: IO ()
main = do
	fun <- getContents >>= makeZot . filter ( not . isSpace ) . remCom
	fun $$$$ output $$$ pr >> putStrLn ""

mainFile :: String -> IO ()
mainFile fn = do
	prg <- readFile fn
	arg <- getContents
	fun <- makeZot $ filter ( not . isSpace ) $ remCom $ prg ++ arg
	fun $$$$ output $$$ pr >> putStrLn ""

makeZot :: String -> IO Fun
makeZot "" = return empty
makeZot bs = do
	f <- makeZot $ init bs
	f $$ case last bs of
		'1'	-> one
		'0'	-> zero
		_	-> error "makeZot: appear not '0' or '1'"

pr :: Fun
pr = Fun $ \x -> interrogate x $$$ Int 0 $$$ Int 1 >>= putStr . show >> return pr

interrogate :: Fun -> IO Fun
interrogate f = f $$ i $$$ i $$$ i $$$ k

output :: IO Fun
output = k $$$$ k $$$$ k $$$$ k $$$$ k $$$$ k $$ i
