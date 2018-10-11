module Main
where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Maybe
import Data.Char
import Debug.Trace

data TestResult
  = TestResult
      { testName :: String
      , testMetrics :: [(Dimension, Integer)]
      }
      deriving (Show)

data Dimension
  = BytesAllocated
  | PeakMegabytesAllocated
  | MaxBytesUsed
  deriving (Show, Eq, Ord, Enum, Bounded)

type P a = Parsec () String a

pTestResults :: P [TestResult]
pTestResults = many (try pTestResult)

pTestResult :: P TestResult
pTestResult = do
  testName <- (skipManyTill pAnyLine pTestHeader)
  skipped <- pAnyLine -- skip one line, which contains the command
  testMetrics <- catMaybes <$> many pMetricLine
  return $ TestResult testName testMetrics

pTestHeader :: P String
pTestHeader = do
  try (string "=====>") *> space1 *> manyTill anyChar (oneOf " \t") <* manyTill anyChar eol

pAnyLine :: P String
pAnyLine = manyTill anyChar eol

pMetricLine :: P (Maybe (Dimension, Integer))
pMetricLine = do
  space1
  what <- manyTill anyChar (oneOf " \t")
  traceM (show what)
  case what of
    "Actual" -> do
      space1
      manyTill anyChar spaceChar -- test name again, we'll ignore this
      dim <- pDimension
      string ":"
      space1
      val <- pInt
      manyTill anyChar eol
      traceM $ show (dim, val)
      return $ Just (dim, val)

    _ -> do
      manyTill anyChar eol
      return Nothing

pDimension :: P Dimension
pDimension
  = (try $ string "bytes allocated" >> pure BytesAllocated)
  <|> (try $ string "peak_megabytes_allocated" >> pure PeakMegabytesAllocated)
  <|> (try $ string "max_bytes_used" >> pure MaxBytesUsed)
  <?> "dimension"

pInt :: P Integer
pInt = read <$> takeWhile1P (Just "int") isDigit

main = do
  src <- getContents
  print $ parse pTestResults "<stdin>" src
