import System.IO (hPutStrLn, stderr)
import System.Exit
import Assemble (assemble)

main :: IO ()
main = do code <- getContents 
          case assemble code of
               Left err -> do hPutStrLn stderr (show err)
                              exitWith $ ExitFailure 1
               Right output -> putStr output
