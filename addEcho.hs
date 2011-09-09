import System.Environment

main = do
	args <- getArgs
	interact (++ concat args)
