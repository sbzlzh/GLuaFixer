import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified GLuaFixer.Interface as GLua
import GLuaFixer.LintSettings (LintSettings, defaultLintSettings)
import System.FilePath (takeBaseName)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension)
import Test.Tasty.Golden.Advanced (goldenTest)

main :: IO ()
main = defaultMain =<< goldenTests

data TestPair = TestPair {inputFile :: FilePath, outputFile :: FilePath}

goldenTests :: IO TestTree
goldenTests = do
  inputFiles <- findByExtension [".lua"] "tests/golden/data/input"
  expectedOutputFiles <- findByExtension [".lua"] "tests/golden/data/output"
  let
    testPairs = zipWith TestPair inputFiles expectedOutputFiles

  pure $
    testGroup
      "Golden pretty print tests"
      [ goldenTest
        (takeBaseName input)
        (LBS.fromStrict <$> BS.readFile output)
        (prettyPrintFile input)
        compareOutput
        (LBS.writeFile output)
      | TestPair input output <- testPairs
      ]

prettyPrintFile :: FilePath -> IO LBS.ByteString
prettyPrintFile inputFile = do
  contents <- normalizeLineEndings <$> readFile inputFile
  let
    Right mtokens = GLua.lex lintSettings inputFile contents
    Right ast = GLua.parse lintSettings inputFile mtokens
    prettyprinted = GLua.prettyprint lintSettings ast

  pure $ LBS8.pack prettyprinted

-- | Git checkouts and text handles can use different line endings on Windows.
compareOutput :: LBS.ByteString -> LBS.ByteString -> IO (Maybe String)
compareOutput expected actual =
  pure $
    if normalizedExpected == normalizedActual
      then Nothing
      else Just $ "Expected:\n" ++ normalizedExpected ++ "\nActual:\n" ++ normalizedActual
  where
    normalizedExpected = normalizeOutput expected
    normalizedActual = normalizeOutput actual

-- | Ignore one optional final newline, while preserving spaces and bare CRs.
normalizeOutput :: LBS.ByteString -> String
normalizeOutput output =
  case reverse $ normalizeLineEndings $ LBS8.unpack output of
    '\n' : rest -> reverse rest
    reversed -> reverse reversed

normalizeLineEndings :: String -> String
normalizeLineEndings ('\r' : '\n' : rest) = '\n' : normalizeLineEndings rest
normalizeLineEndings (char : rest) = char : normalizeLineEndings rest
normalizeLineEndings [] = []

lintSettings :: LintSettings
lintSettings = defaultLintSettings
