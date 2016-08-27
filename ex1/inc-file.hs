import System.IO

numberFile = "./ex1/number.txt"

-- Parses the contents of the number file as an integer, increments it,
-- writes it back to the file and prints it to stdout.
main = do
  newContents <- transformFile numberFile $ show . succ . parseInt
  putStrLn newContents

parseInt :: String -> Integer
parseInt s = read s :: Integer

-- Transforms the contents of a file and returns the new contents
transformFile :: FilePath -> (String ->  String) -> IO String
transformFile filename f = do
  contents <- readFile filename
  let newContents = f contents
  length newContents `seq` writeFile filename newContents
  return newContents
