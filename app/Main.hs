
module Main where

import Dictionary.Csv
import Dictionary.Output
import Dictionary.Search
import Dictionary.Types
import Latex
import System.Environment

-- Filter entries in WaDoku ---------------------------------------------------------------------------------------

{-| Path to WaDoku data. -}
wadoku :: FilePath 
wadoku = "data/NewWaDokuDa.tsv"

{-| Filters the WaDoku dictionary using a given query. -}
filterWadoku :: String -> IO (Either String Dictionary)
filterWadoku query = do 
    dict <- readDictionaryFromFilepath wadoku
    return $ (`filterEntries` query) <$> dict

-- Non-exporting program ------------------------------------------------------------------------------------------

{-| Given a query, runs the program which filters WaDoku and prints the result. -}
runWithoutExport :: String -> IO ()
runWithoutExport query = do
    putStrLn "Searching WaDoku..."
    result <- filterWadoku query
    case result of 
        Left err -> putStrLn err
        Right dict -> putDictionary dict
    putStrLn "Done."

-- Exporting program ----------------------------------------------------------------------------------------------

{-| Has the user choose a dictionary entry in a dictionary by entering a number. -}
selectDictionaryEntry :: Dictionary -> IO (Either String DictionaryEntry)
selectDictionaryEntry dict = case length dict of 
    0 -> return $ Left "No entries in dictionary."
    1 -> return $ Right (dict ! 0) 
    _ -> do 
        putStrLn "Multiple options available, type a number to select:"
        selection <- getLine >>= readIO :: IO Int
        return $ Right (dict ! selection)

{-| Given a query and a LaTeX file to export to, runs the program which filters WaDoku, prints the result, asks 
the user which dictionary entry to export, generates LaTeX code for that entry and appends it to the file -}
{- TODO: It may be better to learn how to use monad transformers to get this creeping indentation out. -}
runWithExport :: FilePath -> String -> IO ()
runWithExport export query = do
    putStrLn "Searching WaDoku..."
    filter <- filterWadoku query
    case filter of 
        Left err -> putStrLn err 
        Right dict -> do
            putDictionary dict
            selection <- selectDictionaryEntry dict
            case selection of 
                Left err -> putStrLn err 
                Right entry -> do
                    putStrLn $ "Appending generated LaTeX code to " ++ export ++ "..."
                    appendDictionaryEntryToLatexFile export entry
                    putStrLn "Done."

-- Main -----------------------------------------------------------------------------------------------------------

{-| Parses the arguments passed to the program, showing a proper usage message if invalid arguments are passed. -}
parseArgs :: [String] -> IO ()
parseArgs [query] = runWithoutExport query
parseArgs ["-o", export, query] = runWithExport export query
parseArgs _ = putStrLn "Usage: japanese-pitch-finder [-o LATEX_FILE] QUERY"

main :: IO ()
main = getArgs >>= parseArgs

-------------------------------------------------------------------------------------------------------------------
