import Assemble (assemble)

main :: IO ()
main = interact (show . assemble)
