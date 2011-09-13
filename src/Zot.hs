module Zot ( main, mainFile ) where

import Data.Char ( isSpace )

data Fun = Fun ( Fun -> IO Fun ) | Zero | One

mkFun :: ( Fun -> Fun ) -> Fun
mkFun f =  Fun $ return . f

cont :: Fun -> Fun
cont = Fun . flip ( $$ )

instance Show Fun where
	show Zero	= "0"
	show One	= "1"
	show ( Fun _ )	= "function"

( $$ ) :: Fun -> Fun -> IO Fun
( $$ ) ( Fun f )	= f
( $$ ) Zero		= ( zero $$ )
( $$ ) One		= ( one $$ )

infixl 7 >$$
( >$$ ) :: IO Fun -> Fun -> IO Fun
f >$$ a = f >>= ( $$ a )

infixr 8 $$<
( $$< ) :: Fun -> IO Fun -> IO Fun
f $$< a = ( f $$ ) =<< a

infixl 6 >$$<
( >$$< ) :: IO Fun -> IO Fun -> IO Fun
f >$$< a = f >>= ( $$< a )

s, k, i :: Fun
s = mkFun $ \x -> mkFun $ \y -> Fun $ \z -> x $$ z >$$< y $$ z
k = mkFun $ mkFun . const
i = mkFun id

empty, zero, one :: Fun
empty	= cont i
zero	= cont $ Fun $ \f -> f $$ s >$$ k
one	= mkFun $ \c -> cont $ mkFun $ \l -> cont $ Fun $ \r -> c $$< l $$ r

makeZot :: String -> IO Fun
makeZot "" = return empty
makeZot bs = do
	f <- makeZot $ init bs
	f $$ case last bs of
		'1'	-> One
		'0'	-> Zero
		_	-> error "makeZot: appear not '0' nor '1'"

pr :: Fun
pr = Fun $ \x -> interrogate x >$$ Zero >$$ One >>= putStr . show >> return pr

interrogate :: Fun -> IO Fun
interrogate f = f $$ i >$$ i >$$ i >$$ k

output :: IO Fun
output = k $$< k $$< k $$< k $$< k $$< k $$ i

readPrint :: String -> IO ()
readPrint src = do
	fun <- makeZot $ filter ( not . isSpace ) $ remCom src
	fun $$< output >$$ pr >> putStrLn ""
	where
	remCom = unlines . map ( takeWhile ( /= '#' ) ) . lines

main :: IO ()
main = getContents >>= readPrint

mainFile :: String -> IO ()
mainFile fn = do
	prg <- readFile fn
	arg <- getContents
	readPrint $ prg ++ arg
