module Zot ( main, mainFile ) where

import Data.Char ( isSpace )

data Fun = Fun ( Fun -> IO Fun ) | Zero | One

instance Show Fun where
	show Zero	= "0"
	show One	= "1"
	show ( Fun _ )	= "function"

apply :: Fun -> Fun -> IO Fun
apply ( Fun f )	= f
apply Zero	= apply zero
apply One	= apply one

( $$ ) :: Fun -> Fun -> IO Fun
( $$ ) = apply

infixl 7 >$$
( >$$ ) :: IO Fun -> Fun -> IO Fun
f >$$ a = f >>= ( $$ a )

infixr 8 $$<
( $$< ) :: Fun -> IO Fun -> IO Fun
f $$< a = ( f $$ ) =<< a

infixl 6 >$$<
( >$$< ) :: IO Fun -> IO Fun -> IO Fun
f >$$< a = f >>= ( $$< a )

retFun :: ( Fun -> IO Fun ) -> IO Fun
retFun = return . Fun

cont :: Fun -> Fun
cont = Fun . flip ( $$ )

retCont :: Fun -> IO Fun
retCont = return . cont

s, k, i :: Fun
s = Fun $ \x -> retFun $ \y -> retFun $ \z -> x $$ z >$$< y $$ z
k = Fun $ retFun . const . return
i = Fun return

empty, zero, one :: Fun
empty	= cont i
zero	= cont $ Fun $ \f -> f $$ s >$$ k
one	= Fun $ \c -> retCont $	Fun $ \l -> retCont $ Fun $ \r -> c $$< l $$ r

remCom :: String -> String
remCom = unlines . map ( takeWhile ( /= '#' ) ) . lines

main :: IO ()
main = do
	fun <- getContents >>= makeZot . filter ( not . isSpace ) . remCom
	fun $$< output >$$ pr >> putStrLn ""

mainFile :: String -> IO ()
mainFile fn = do
	prg <- readFile fn
	arg <- getContents
	fun <- makeZot $ filter ( not . isSpace ) $ remCom $ prg ++ arg
	fun $$< output >$$ pr >> putStrLn ""

makeZot :: String -> IO Fun
makeZot "" = return empty
makeZot bs = do
	f <- makeZot $ init bs
	f $$ case last bs of
		'1'	-> One -- one
		'0'	-> Zero -- zero
		_	-> error "makeZot: appear not '0' or '1'"

pr :: Fun
pr = Fun $ \x -> interrogate x >$$ Zero >$$ One >>= putStr . show >> return pr

interrogate :: Fun -> IO Fun
interrogate f = f $$ i >$$ i >$$ i >$$ k

output :: IO Fun
output = k $$< k $$< k $$< k $$< k $$< k $$ i
