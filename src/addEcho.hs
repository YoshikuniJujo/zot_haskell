import System.Environment
import qualified AddEcho

main = getArgs >>= AddEcho.main
