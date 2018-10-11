{-#LANGUAGE LambdaCase #-}
module Main
where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Maybe
import Data.Char
import Debug.Trace
import Control.Monad
import Text.Printf
import Data.Map (Map)
import qualified Data.Map as Map
import System.Environment
import Data.Void

data Dimension
  = BytesAllocated
  | PeakMegabytesAllocated
  | MaxBytesUsed
  deriving (Show, Eq, Ord, Enum, Bounded)

dimensionName :: Dimension -> String
dimensionName BytesAllocated = "bytes alloc"
dimensionName PeakMegabytesAllocated = "peak MB alloc"
dimensionName MaxBytesUsed = "max bytes used"

type TestName = String

type TestResults a = Map (TestName, Dimension) a

type P a = Parsec Void String a

pTestResults :: P (TestResults Integer)
pTestResults = do
  mconcat . catMaybes <$> many (try pMetricLine <|> (Nothing <$ pAnyLine))

pAnyLine :: P String
pAnyLine = manyTill anySingle eol

pMetricLine :: P (Maybe (TestResults Integer))
pMetricLine = do
  space1
  what <- manyTill anySingle (oneOf " \t")
  case what of
    "Actual" -> do
      space1
      testName <- manyTill anySingle spaceChar
      dim <- pDimension
      string ":"
      space1
      val <- pInt
      manyTill anySingle eol
      return $ Just (Map.singleton (testName, dim) val)

    _ -> do
      manyTill anySingle eol
      return Nothing

pDimension :: P Dimension
pDimension
  = (try $ string "bytes allocated" >> pure BytesAllocated)
  <|> (try $ string "peak_megabytes_allocated" >> pure PeakMegabytesAllocated)
  <|> (try $ string "max_bytes_used" >> pure MaxBytesUsed)
  <?> "dimension"

pInt :: P Integer
pInt = read <$> takeWhile1P (Just "int") isDigit

runStdin :: IO ()
runStdin = do
  src <- getContents
  readResults "<stdin>" src >>= reportOne "-"

runOne :: String -> IO ()
runOne fn = do
  src <- readFile fn
  readResults fn src >>= reportOne fn

reportOne :: String -> TestResults Integer -> IO ()
reportOne fn results = do
  printf "%-22s %-15s %15s\n" "test" "type" (cutTo 15 fn)
  putStrLn $ replicate 54 '-'
  forM_ (Map.toAscList results) $ \((testName, dim), val) -> do
    printf "%-22s %-15s %15i\n" (cutTo 22 testName) (dimensionName dim) val

runTwo :: String -> String -> IO ()
runTwo fn1 fn2 = do
  src1 <- readFile fn1
  src2 <- readFile fn2
  results1 <- readResults fn1 src1
  results2 <- readResults fn2 src2
  reportTwo (fn1, fn2) (Map.intersectionWith (,) results1 results2)

reportTwo :: (String, String) -> TestResults (Integer, Integer) -> IO ()
reportTwo (fn1, fn2) results = do
  printf "%-22s %-15s %15s %15s %9s\n" "test" "type" (cutTo 15 fn1) (cutTo 15 fn2) "dev."
  putStrLn $ replicate 80 '-'
  forM_ (Map.toAscList results) $ \((testName, dim), (val1, val2)) -> do
    let deviation = fromIntegral val2 * 100 / fromIntegral val1 - 100 :: Double
    printf "%-22s %-15s %15i %15i %+8.1f%%\n"
      (cutTo 22 testName)
      (dimensionName dim)
      val1 val2 deviation

cutTo :: Int -> String -> String
cutTo n str
  | length str > n =
      let pfl = (n-3) `div` 2
      in take pfl str ++ "..." ++ drop (length str - n + 3 + pfl) str
  | otherwise = str

readResults :: FilePath -> String -> IO (TestResults Integer)
readResults srcName src = do
  case parse pTestResults srcName src of
    Left err -> error . errorBundlePretty $ err
    Right results -> return results

main = do
  getArgs >>= \case
    [] -> runStdin
    [fn] -> runOne fn
    [a,b] -> runTwo a b
    _ -> error "Invalid arguments"
