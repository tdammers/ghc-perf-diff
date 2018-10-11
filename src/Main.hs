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

data Dimension
  = BytesAllocated
  | PeakMegabytesAllocated
  | MaxBytesUsed
  deriving (Show, Eq, Ord, Enum, Bounded)

type TestName = String

type TestResults = Map (TestName, Dimension) Integer

type P a = Parsec () String a

pTestResults :: P TestResults
pTestResults = do
  mconcat . catMaybes <$> many (try pMetricLine <|> (Nothing <$ pAnyLine))

pAnyLine :: P String
pAnyLine = manyTill anySingle eol

pMetricLine :: P (Maybe TestResults)
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
      traceM $ show (testName, dim, val)
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

main = do
  src <- getContents
  case parse pTestResults "<stdin>" src of
    Left err -> print err
    Right results -> mapM_ print $ Map.toList results
