module App where

import Args qualified
import Data.List qualified as L
import Debug.Trace
import Html qualified as H
import Html.Parser qualified as H
import Html.Parser qualified as Q
import Parser qualified as P
import Query qualified as Q
import Query.Parser qualified as Q
import Result (Result (..))
import Result qualified
import System.Environment qualified as ENV
import Test.SimpleTest.Mock (TestableMonadIO (..))
import Prelude hiding (getContents, print, putStrLn, readFile)

data MainError = FileNotFound FilePath | ParseError FilePath P.ParseError deriving (Eq, Show)

data FileMatch = FileMatch FilePath H.Html deriving (Eq)

instance Show FileMatch where
  show (FileMatch path html) = path ++ "\n" ++ show html

main' :: (TestableMonadIO io) => String -> [String] -> io ()
main' progName argList = case Args.parseArgs progName argList of
  Success (Args.Help usage) -> putStrLn usage
  Success
    ( Args.Args
        { Args.argQuery = query,
          Args.argFiles = files,
          Args.argMaxResults = maxResults
        }
      ) -> do
      contents <- readContents files
      let results = searchFiles query <$> (contents >>= parseContents)
      case results of
        Success matches -> printMatches (maybe id take maxResults matches)
        Error err -> print err
      return ()
  Error err -> putStrLn "Failed to parse args"

-- Prints each match using @show@, separated by 2 newlines ("\n\n")
printMatches :: (TestableMonadIO io) => [FileMatch] -> io ()
printMatches = mapM_ (\(FileMatch path html) -> putStrLn (path ++ "\n" ++ show html ++ "\n\n"))

-- | Returns a tuple list of @(FilePath, contents)@.
--
-- If the first argument is `Args.Stdin`, the contents of stdin is returned (i.e. @[("stdin", contents of stdin)]@).
-- Hint: To obtain the contents of stdin, use `getContents`.
--
-- If the first argument is @Args.Files (list of files)@, the contents of each is file is returned together with the path of the file
-- (i.e. for @Args.Files ["test1.html", "test2.html"]@, (assuming both files exist) the result should be @[("test1.html", contents of test1.html), ("test2.html", contents of test2.html)]@)
--
-- If one of the files is not found, it returns @Error FileNotFound@ with the path of the missing file.
-- (i.e. for @Args.Files ["test1.html", "invalid.html"]@, if invalid.html does not exist, the result should be @Error FileNotFound "invalid.html"@)
readContents :: (TestableMonadIO io) => Args.SearchedFiles -> io (Result MainError [(FilePath, String)])
readContents Args.Stdin =
  do
    contents <- getContents
    return (Success [("stdin", contents)])
readContents (Args.Files fileNames) = do
  results <- traverse readContentFile fileNames
  let combinedResults = combineResults results
  return combinedResults
  where
    combineResults :: [Result MainError (FilePath, String)] -> Result MainError [(FilePath, String)]
    combineResults results = case sequence results of
      Success xs -> Success xs
      Error err -> Error err

    readContentFile :: (TestableMonadIO io) => FilePath -> io (Result MainError (FilePath, String))
    readContentFile fileName = do
      result <- maybeReadFile fileName
      case result of
        Success content -> return (Success (fileName, content))
        Error err -> return (Error (FileNotFound fileName))

-- | Tries to read a file, returning its contents in the `Success` wrapper.
-- If the file doesn't exist it returns @Error FileNotFound path@
maybeReadFile :: (TestableMonadIO io) => FilePath -> io (Result MainError String)
maybeReadFile path = do
  exists <- doesFileExist path
  if exists
    then do
      contents <- readFile path
      return (Success contents)
    else return (Error (FileNotFound path))

-- | Given a list of file paths and their contents, tries to parse the contents of each file.
--
-- If the parsing fails for any file, it returns @Error (path first invalid file) (parse error)@
--
-- >>> parseContents [("test.html", "<div></div>")]
-- Success [("test.html",Document [Node (HtmlNode {nodeTag = "div", nodeAttrs = [], nodeChildren = []})])]
--
-- >>> parseContents [("good1.html", "<p></p>"), ("bad.html", "<div><div>")]
-- Error (ParseError "bad.html" (UnexpectedInput {gotInput = "<div><div>", expectedInput = "At least one At least one text or self closing tag or "}))
parseContents :: [(FilePath, String)] -> Result MainError [(FilePath, H.Document)]
parseContents = traverse parseFile
  where
    parseFile :: (FilePath, String) -> Result MainError (FilePath, H.Document)
    parseFile (filePath, fileContent) =
      case H.parse fileContent of
        Success document -> Success (filePath, document)
        Error parseError -> Error (ParseError filePath parseError)

-- | Given a query and a list of file paths and their parsed contents, searches for matches for the query.
--
-- Note that the @search@ function receives one @H.Document@, and returns a list of @H.Html@ nodes that contain the matches,
-- while the @searchFiles@ function receives a list of @H.Document@ and returns a list of @FileMatch@.
--
-- Some useful functions:
-- - @search@
searchFiles :: Q.Query -> [(FilePath, H.Document)] -> [FileMatch]
searchFiles query = foldr searchFile []
  where
    searchFile :: (FilePath, H.Document) -> [FileMatch] -> [FileMatch]
    searchFile (path, doc) acc = case search query doc of
      [] -> acc
      matches -> map (FileMatch path) matches ++ acc

search :: Q.Query -> H.Document -> [H.Html]
search query (H.Document nodes) = searchNodes True query nodes

searchNodes :: Bool -> Q.Query -> [H.Html] -> [H.Html]
searchNodes _ _ [] = []
searchNodes recursive query ((H.Text _) : rest) = searchNodes recursive query rest
searchNodes recursive query ((H.Node node) : rest) = searchNode recursive query node ++ searchNodes recursive query rest

searchNode :: Bool -> Q.Query -> H.HtmlNode -> [H.Html]
searchNode recursive query node = case query of
  (Q.Selector selector) -> queryMatches recursive selector node
  (Q.Descendant query1 query2) -> searchNodes True query2 $ childrenOfMatchedNodes recursive query1 node
  (Q.Child query1 query2) -> searchNodes False query2 $ childrenOfMatchedNodes recursive query1 node
  (Q.List query1 query2) -> searchNode recursive query1 node ++ searchNode recursive query2 node

queryMatches :: Bool -> Q.QuerySelector -> H.HtmlNode -> [H.Html]
queryMatches recursive selector node@(H.HtmlNode {H.nodeTag = nodeTag, H.nodeChildren = nodeChildren, H.nodeAttrs = nodeAttrs})
  | matchesNode selector node = [H.Node node]
  | recursive = searchNodes recursive (Q.Selector selector) nodeChildren
  | otherwise = []

childrenOfMatchedNodes :: Bool -> Q.Query -> H.HtmlNode -> [H.Html]
childrenOfMatchedNodes recursive query node = [directChild | H.Node match <- searchNode recursive query node, directChild <- H.nodeChildren match]

matchesNode :: Q.QuerySelector -> H.HtmlNode -> Bool
matchesNode selector node = maybe True (== nodeTag) tag && all (`elem` nodeAttrs) queryAttrs
  where
    (Q.QuerySelector {Q.selectorTag = tag, Q.selectorIds = ids, Q.selectorClasses = classes, Q.selectorAttributes = attrs}) = selector
    (H.HtmlNode {H.nodeTag = nodeTag, H.nodeChildren = nodeChildren, H.nodeAttrs = nodeAttrs}) = node
    queryAttrs = Q.allAttrs selector
