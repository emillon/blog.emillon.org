import System.Cmd
import System.Exit

main :: IO ()
main = do
    status <- system "./dist/build/blog/blog build"
    exitWith status
