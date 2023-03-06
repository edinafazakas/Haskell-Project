module Main where
import Args
  ( AddOptions (..),
    Args (..),
    GetOptions (..),
    SearchOptions (..),
    parseArgs,
  )
import qualified Data.List as L
import qualified Entry.DB as DB
import Entry.Entry
  ( Entry (..),
    FmtEntry (FmtEntry),
    matchedByAllQueries,
    matchedByQuery,
  )
import Result ( Result(Success, Error) )
import System.Environment (getArgs)
import Test.SimpleTest.Mock ( TestableMonadIO(putStrLn, readFile) )
import Prelude hiding (print, putStrLn, readFile)
import qualified Prelude
import Entry.DB (empty, SnippetDB)
import Distribution.Types.InstalledPackageInfo (emptyInstalledPackageInfo)
import qualified Data.Type.Bool as Entry

usageMsg :: String
usageMsg =
  L.intercalate
    "\n"
    [ "snip - code snippet manager",
      "Usage: ",
      "snip add <filename> lang [description] [..tags]",
      "snip search [code:term] [desc:term] [tag:term] [lang:term]",
      "snip get <id>",
      "snip init"
    ]

-- | Handle the init command
handleInit :: TestableMonadIO m => m()
handleInit = 
   do
    DB.save (empty :: SnippetDB) 
    return()

-- | Handle the get command
handleGet :: TestableMonadIO m => GetOptions -> m ()
handleGet (GetOptions id) = do
  dbOrError <- DB.load
  case dbOrError of
    (Error _) -> putStrLn "Failed to load DB"
    (Success db) -> 
      case DB.findFirst (\e -> entryId e == id) db of
        Just entry -> putStrLn $ entrySnippet entry
        Nothing -> putStrLn "Error: entry not found"


-- | Handle the search command
handleSearch :: TestableMonadIO m => SearchOptions -> m ()
handleSearch (SearchOptions terms) = do
    dbOrError <- DB.load
    case dbOrError of
        (Error _) -> putStrLn "Failed to load DB"
        (Success db) -> 
            let matchedEntries = DB.findAll (matchedByAllQueries terms) db
                in if (length matchedEntries) == 0
                    then putStrLn "No entries found"
                    else mapM_ (putStrLn . show . FmtEntry) matchedEntries



-- | Handle the add command
handleAdd :: TestableMonadIO m => AddOptions -> m ()
handleAdd addOpts = do
  db <- DB.load
  case db of
    (Success snippetDB) -> do
      contents <- readFile (addOptFilename addOpts)
      let entryExists = DB.findFirst (\e -> entrySnippet e == contents) snippetDB
      case entryExists of
        Just entry -> do 
          putStrLn $ "Entry with this content already exists: " 
          putStrLn $ "[" ++ show (entryId entry) ++ "] " ++ (entryFilename entry)  ++ ":"
        Nothing -> do 
          let newDb = DB.insertWith (\id -> makeEntry id contents addOpts) snippetDB
          result <- DB.save newDb
          case result of
            Success _ -> putStrLn "New entry added successfully"
            Error _ -> putStrLn "Error adding new entry"
    (Error _) -> putStrLn "Failed to load DB"
  where
      makeEntry :: Int -> String -> AddOptions -> Entry
      makeEntry id snippet addOpts =
          Entry
              { entryId = id,
                entrySnippet = snippet,
                entryFilename = addOptFilename addOpts,
                entryLanguage = addOptLanguage addOpts,
                entryDescription = addOptDescription addOpts,
                entryTags = addOptTags addOpts
              }

                

                
-- | Dispatch the handler for each command
run :: TestableMonadIO m => Args -> m ()
run (Add addOpts) = handleAdd addOpts
run (Search searchOpts) = handleSearch searchOpts
run (Get getOpts) = handleGet getOpts
run Init = handleInit
run Help = putStrLn usageMsg

main :: IO ()
main = do
  args <- getArgs
  let parsed = parseArgs args
  case parsed of
    (Error err) -> Prelude.putStrLn usageMsg
    (Success args) -> run args
