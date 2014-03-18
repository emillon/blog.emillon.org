import System.Cmd
import System.Exit

main :: IO ()
main = do
    status <- system "linkchecker --check-html --check-css --anchors _site/index.html"
    exitWith status
