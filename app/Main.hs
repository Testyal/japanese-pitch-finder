module Main where

import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import System.Environment
import System.Exit
import Search
import Downstep
import TeXGenerator

parseArgs :: [String] -> IO ()
parseArgs [query] = mainNoOutputNew query
parseArgs ["-o", file, query] = mainWithOutputNew query file
parseArgs _ = do
    putStrLn "usage: stack run WORD_TO_FIND [-o OUTPUT_FILE]" 
    exitFailure >>= putStrLn 

main :: IO ()
main = getArgs >>= parseArgs

-- TODO: Remove creeping indentation     
mainNoOutput :: String -> IO ()
mainNoOutput query = do
    wadokuDictionary <- BL.readFile "data/WaDokuDa.tsv"
    case findJapaneseEntryInCsv wadokuDictionary query of 
        Left err -> putStrLn err
        Right entry -> do 
            putDictionaryEntry entry
            case applyDownstepToDictionaryEntry entry of 
                Left err -> putStrLn err 
                Right pronunciation -> do
                    putPronunciation pronunciation
                    putStrLn $ generateTeXDictionaryEntry query pronunciation

mainNoOutputNew :: String -> IO ()
mainNoOutputNew query = do 
    wadoku <- BL.readFile "data/NewWaDokuDa.tsv"
    case convertCsvToDictionary wadoku of
        Left err -> putStrLn err
        Right dict -> putDictionaryWithPronunciation $ filterJapaneseEntries dict query

-- TODO: Remove creeping indentation
mainWithOutput :: String -> FilePath -> IO ()
mainWithOutput query file = do
    wadokuDictionary <- BL.readFile "data/WaDokuDa.tsv"
    case findJapaneseEntryInCsv wadokuDictionary query of 
        Left err -> putStrLn err
        Right entry -> do 
            putDictionaryEntry entry
            case applyDownstepToDictionaryEntry entry of 
                Left err -> putStrLn err 
                Right pronunciation -> do
                    putPronunciation pronunciation
                    putStrLn $ generateTeXDictionaryEntry query pronunciation
                    appendFile file $ "\n\n" ++ generateTeXDictionaryEntry query pronunciation

-- FIXME: This is disgusting
mainWithOutputNew :: String -> FilePath -> IO ()
mainWithOutputNew query file = do 
    putStrLn "Searching for entries..."
    wadoku <- BL.readFile "data/NewWaDokuDa.tsv"
    case convertCsvToDictionary wadoku of 
        Left err -> putStrLn err 
        Right dict -> do
            putDictionaryWithPronunciation filteredDict
            case V.length filteredDict of 
                0 -> return ()
                1 -> case applyDownstepToDictionaryEntry (filteredDict V.! 0) of 
                    Left err -> putStrLn err 
                    Right pronunciation -> do
                        putStrLn $ "Appending " ++ texEntry ++ "　to " ++ file ++ "..."
                        appendFile file $ "\n\n" ++ texEntry
                        putStrLn "Done."
                        where
                            texEntry = generateTeXDictionaryEntry query pronunciation
                _ -> do 
                    putStrLn "Multiple options encountered. Type a number to select: " 
                    selection <- getLine >>= readIO :: IO Int
                    case applyDownstepToDictionaryEntry (filteredDict V.! (selection - 1)) of 
                        Left err -> putStrLn err 
                        Right pronunciation -> do
                            putStrLn $ "Appending " ++ texEntry' ++ " to " ++ file ++ "..."
                            appendFile file $ "\n\n" ++ texEntry'
                            putStrLn "Done."
                            where
                                texEntry' = generateTeXDictionaryEntry query pronunciation
            where 
                filteredDict = filterJapaneseEntries dict query


