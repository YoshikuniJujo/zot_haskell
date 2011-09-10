import qualified SkiToLambda
import System.Environment ( getArgs )

main :: IO ()
main = getArgs >>= SkiToLambda.main
