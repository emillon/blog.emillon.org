import Control.Monad
import System.Cmd
import System.Exit

runOrFail :: String -> IO ()
runOrFail cmd = do
    status <- system cmd
    when (status /= ExitSuccess) $ exitWith status

main :: IO ()
main = do
    runOrFail "./dist/build/blog/blog build"
    runOrFail "linkchecker --check-html --check-css --anchors _site/index.html"
