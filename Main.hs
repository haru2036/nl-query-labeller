import Text.CSV
import Text.Nagato.MeCabTools
import System.IO

main :: IO()
main = label

label :: IO()
label = do
  putStrLn "Please type sentence for register"
  sentence <- getLine
  parsed <- parseFormat sentence
  mapM (putStrLnUtf8 stdout) $ lineNumbers $ lines parsed
  putStrLn "Please type part numbers (delimiter is space)"
  input <- getLine
  let answer = inputToAnswers input
  let record = sentence : (map show answer)
  appendCSV record

inputToAnswers  :: String -> [Int]
inputToAnswers = (map read)  . words

lineNumbers :: [String] -> [String]
lineNumbers = joinNumbers . zip [0..] 

joinNumbers :: [(Int, String)] -> [String]
joinNumbers = map joinNumber

joinNumber :: (Int, String) -> String
joinNumber itm = (show (fst itm)) ++ " " ++ (snd itm)

putStrLnUtf8 :: Handle -> String -> IO()
putStrLnUtf8 handle s = do
  hSetEncoding handle utf8
  hPutStrLn handle s

readFileUtf8 :: Handle -> IO String
readFileUtf8 handle = do 
  hSetEncoding handle utf8
  hGetContents handle

readCSV :: IO CSV
readCSV = do
  fileHandle <- openFile "setting.csv" ReadWriteMode
  string <- readFileUtf8 fileHandle 
  hClose fileHandle
  return $ eitherCSV $ parseCSV "setting.csv" string

eitherCSV :: Either t [t1] -> [t1]
eitherCSV (Right contents) = contents
eitherCSV (Left _) = []

appendCSV :: Record -> IO()
appendCSV record = do
  inCSV <- readCSV
  let newCSV = record : inCSV 
  fileHandle <- openFile "setting.csv" ReadWriteMode
  putStrLnUtf8 fileHandle  $ printCSV newCSV
  hClose fileHandle
