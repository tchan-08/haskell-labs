import System.IO
import System.Environment 
import System.Random 
main :: IO ()

-- 2.1

helloWorld :: IO()
helloWorld = putStrLn "Hello, World!"

-- 2.2

greet :: IO()
greet = do
  response <- getLine
  putStrLn response

-- 2.3

getInt :: IO Int
getInt = do
  response <- getLine
  return (read response)

-- 3.1 

addLineString :: [String] -> [String]
addLineString [] = []
addLineString xs = [">" ++ x | x <- xs]

-- 3.2

addLineStringToContents :: String -> String
addLineStringToContents str = do
  let lineList = lines str
  unlines (addLineString lineList)

-- 3.3

hsToLhs :: FilePath -> FilePath -> IO()
hsToLhs hsFile lhsFile= do
  hsHandle <- openFile hsFile ReadMode
  contents <- hGetContents hsHandle
  let toWrite = addLineStringToContents contents
  writeFile lhsFile toWrite
  hClose hsHandle

-- 4

echoArgs :: IO()
echoArgs = do
  args <- getArgs
  print (unwords args)

-- 5.1

playRound :: Int -> IO ()
playRound target = do
  guessStr <- getLine
  let guessInt = read guessStr
  if guessInt == target then
    putStrLn "You got it right!"
  else if guessInt < target then do
    putStrLn("Guess is too low!")
    playRound target
  else do
    putStrLn("Guess is too high!")
    playRound target

-- 5.2

dice :: Int -> IO Int
dice n = do
  random <- randomIO
  let roll = random `mod` n
  return roll

-- 5.3

game :: Int -> IO ()
game n = do
  target <- dice n
  playRound target

-- 6.1

sequential :: [IO a] -> IO [a]
sequential [] = return []
sequential (x:xs) = do
  a <- x
  as <- sequential xs
  return (a : as)

-- 6.2

getManyLines :: Int -> IO [String]
getManyLines n = do
  let fList = replicate n getLine
  sequential fList 

main = do echoArgs
