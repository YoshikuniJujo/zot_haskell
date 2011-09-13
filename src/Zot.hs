module Zot ( main, mainFile ) where

import Control.Monad ( foldM )

data Fun = Fun ( Fun -> IO Fun ) | Zero | One

mkFun :: ( Fun -> Fun ) -> Fun
mkFun f =  Fun $ return . f

instance Show Fun where
	show Zero	= "0"
	show One	= "1"
	show ( Fun _ )	= "function"
	showList fs	= ( concatMap show fs ++ )

instance Read Fun where
	readsPrec _ ( '0' : rest )	= [ ( Zero, rest ) ]
	readsPrec _ ( '1' : rest )	= [ ( One, rest ) ]
	readsPrec _ _			= error "read Fun failed"
	readList str			= [ readl str ]
		where
		readl ""		= ( [ ], "" )
		readl ( '0' : cs )	= let ( r, rest ) = readl cs in
						( Zero : r, rest )
		readl ( '1' : cs )	= let ( r, rest ) = readl cs in
						( One : r, rest )
		readl _			= error "read [ Fun ] failed"

( $$ ) :: Fun -> Fun -> IO Fun
( $$ ) ( Fun f )	= f
( $$ ) Zero		= ( zero $$ )
( $$ ) One		= ( one $$ )

infixl 6 >$$
( >$$ ) :: IO Fun -> Fun -> IO Fun
f >$$ a = f >>= ( $$ a )

infixr 8 $$<
( $$< ) :: Fun -> IO Fun -> IO Fun
f $$< a = ( f $$ ) =<< a

infixl 7 >$$<
( >$$< ) :: IO Fun -> IO Fun -> IO Fun
f >$$< a = f >>= ( $$< a )

cont :: Fun -> Fun
cont = Fun . flip ( $$ )

s, k, i :: Fun
s = mkFun $ \x -> mkFun $ \y -> Fun $ \z -> x $$ z >$$< y $$ z
k = mkFun $ mkFun . const
i = mkFun id

empty, zero, one :: Fun
empty	= cont i
zero	= cont $ Fun $ \f -> f $$ s >$$ k
one	= mkFun $ \c -> cont $ mkFun $ \l -> cont $ Fun $ \r -> c $$< l $$ r

pr :: Fun
pr = Fun $ \x -> interrogate x >$$ Zero >$$ One >>= putStr . show >> return pr

interrogate :: Fun -> IO Fun
interrogate f = f $$ i >$$ i >$$ i >$$ k

output :: IO Fun
output = k $$< k $$< k $$< k $$< k $$< k $$ i

readZot :: String -> IO Fun
readZot = foldM ( $$ ) empty . read . filter ( `elem` "01" )

interpret :: String -> IO ()
interpret src = readZot ( removeComment src ) >$$< output >$$ pr >> putStrLn ""
	where
	removeComment = concat . map ( takeWhile ( /= '#' ) ) . lines

main :: IO ()
main = getContents >>= interpret

mainFile :: String -> IO ()
mainFile fn = do
	prg <- readFile fn
	arg <- getContents
	interpret $ prg ++ arg
