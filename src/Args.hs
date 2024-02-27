module Args where

import Data.List
import Query qualified as Q
import Query.Parser qualified as Q
import Result
import Text.Read (readMaybe)

data SearchedFiles
  = Stdin
  | Files [String]
  deriving (Eq, Show)

data Args
  = Args
      { -- | Given as the first argument
        argQuery :: Q.Query,
        -- | Rest of arguments
        argFiles :: SearchedFiles,
        -- | Given as @--max-results value@
        argMaxResults :: Maybe Int
      }
  | Help String
  deriving (Eq, Show)

data ParseArgsError
  = NotEnoughArgs
  | InvalidArgs String
  deriving (Eq, Show)

usage progName = "Usage: " ++ progName ++ " query pattern [files...]"

-- | Parses the program arguments
--
-- First parameter is the name of the executable. The second parameter (of type @[String])@) contains the list of arguments.
--
-- The first positional argument represents the query and the rest of the arguments represent the files to check.
-- If no files are provided (i.e. only one argument is given, which represents the query), stdin should be used (i.e. the @Stdin@ constructor for @SearchedFiles@).
-- The @--max-results value@ flag is optional, and might be given in any position.
-- If the -h or --help arguments are provided, the function should return @Success@ and the usage in the @Help@ constructor
--
-- Useful functions:
-- - @separateFlags@
-- - @Query.Parser.parse@
--
-- >>> parseArgs "html-search.exe" ["-h"]
-- Success (Help "Usage: html-search.exe query pattern [files...]")
--
-- >>> parseArgs "html-search.exe" ["div > h1.title", "file.html"]
-- Success (Args {argQuery = Child (Selector (QuerySelector {selectorTag = Just "div", selectorIds = [], selectorClasses = [], selectorAttributes = []})) (Selector (QuerySelector {selectorTag = Just "h1", selectorIds = [], selectorClasses = ["title"], selectorAttributes = []})), argFiles = Files ["file.html"], argMaxResults = Nothing})
--
-- >>> parseArgs "html-search.exe" ["div > h1"]
-- Success (Args {argQuery = Child (Selector (QuerySelector {selectorTag = Just "div", selectorIds = [], selectorClasses = [], selectorAttributes = []})) (Selector (QuerySelector {selectorTag = Just "h1", selectorIds = [], selectorClasses = [], selectorAttributes = []})), argFiles = Stdin, argMaxResults = Nothing})
--
-- >>> parseArgs "html-search.exe" ["div h1", "file1.html", "file2.html"]
-- Success (Args {argQuery = Descendant (Selector (QuerySelector {selectorTag = Just "div", selectorIds = [], selectorClasses = [], selectorAttributes = []})) (Selector (QuerySelector {selectorTag = Just "h1", selectorIds = [], selectorClasses = [], selectorAttributes = []})), argFiles = Files ["file1.html","file2.html"], argMaxResults = Nothing})
--
-- >>> parseArgs "html-search.exe" ["div > h1", "file.html", "--max-results", "1"]
-- Success (Args {argQuery = Child (Selector (QuerySelector {selectorTag = Just "div", selectorIds = [], selectorClasses = [], selectorAttributes = []})) (Selector (QuerySelector {selectorTag = Just "h1", selectorIds = [], selectorClasses = [], selectorAttributes = []})), argFiles = Files ["file.html"], argMaxResults = Just 1})
--
-- >>> parseArgs "html-search.exe" ["div > h1", "--max-results", "1", "file.html"]
-- Success (Args {argQuery = Child (Selector (QuerySelector {selectorTag = Just "div", selectorIds = [], selectorClasses = [], selectorAttributes = []})) (Selector (QuerySelector {selectorTag = Just "h1", selectorIds = [], selectorClasses = [], selectorAttributes = []})), argFiles = Files ["file.html"], argMaxResults = Just 1})
parseArgs :: String -> [String] -> Result ParseArgsError Args
parseArgs name args
  | "--help" `elem` args || "-h" `elem` args = Success (Help (usage name))
  | otherwise = case separateFlags args of
      Error err -> Error err
      Success (flags, positionals) -> case positionals of
        [] -> Error NotEnoughArgs
        (query : files) ->
          case Q.parse query of
            Error err -> Error (InvalidArgs (show err))
            Success parsedQuery -> handleResults parsedQuery files flags

handleResults :: Q.Query -> [String] -> [(String, String)] -> Result ParseArgsError Args
handleResults query files flags = do
  let searchedFiles = if null files then Stdin else Files files
  let maxResultsFlag = lookup "--max-results" flags
  maxResults <- case maxResultsFlag of
    Just value -> case readMaybe value of
      Just results -> return (Just results)
      Nothing -> Error (InvalidArgs "Invalid max results")
    Nothing -> return Nothing
  return (Args query searchedFiles maxResults)

-- | Separates positional arguments and flags
--
-- >>> separateFlags ["--flag1", "value1", "--flag2", "value2", "positional1"]
--
-- >>> separateFlags ["positional1", "--flag1", "value1", "--flag2", "value2"]
separateFlags :: [String] -> Result ParseArgsError ([(String, String)], [String])
separateFlags [] = Success ([], [])
separateFlags [last] =
  if "--" `isPrefixOf` last
    then Error $ InvalidArgs "Flag without value"
    else Success ([], [last])
separateFlags (arg : value : rest)
  | "--" `isPrefixOf` arg = do
      (flags, positionals) <- separateFlags rest
      return ((arg, value) : flags, positionals)
  | otherwise = do
      (flags, positionals) <- separateFlags (value : rest)
      return (flags, arg : positionals)
