import System.IO (hPutStrLn, stderr)
import System.Exit
import Control.Exception.Base (displayException)
import Assemble (assemble)

main :: IO ()
main = do code <- getContents 
          case assemble code of
               Left err -> do hPutStrLn stderr (displayException err)
                              exitWith $ ExitFailure 1
               Right output -> putStr output
