module AddEcho ( main ) where

import System.Environment

main args = do
--	args <- getArgs
	interact (++ concat args)
