
-- TODO: It would be nice to have a printing module/library where the margins were automatically added and I could put numbers in them
module Dictionary.Output where

import Data.Maybe
import qualified Data.Vector as V
import Dictionary.Types

-- Output for DictionaryEntry ------------------------------------------------------------------------------------------

{-| Puts a dictionary entry on the screen, listing the word (in Japanese), its reading without pitch, its accent number
if available, and its reading with pitch marks added -}
putDictionaryEntry :: DictionaryEntry -> IO ()
putDictionaryEntry entry = do
    putStrLn $ "| Japanese: " ++ japanese entry
    putStrLn $ "| Reading without pitch: " ++ reading entry
    case accent entry of
        Nothing -> putStrLn "| Accent is not available."
        Just a -> do
            putStrLn $ "| Accent appears on mora " ++ show a
            putStrLn $ "| Reading with pitch: " ++ (showPronunciation . fromJust . pronunciation $ entry)

-- Output for Dictionary -----------------------------------------------------------------------------------------------

{-| Puts a dictionary on the screen in the form of a list of dictionary entries with their number in the list shown -}
putDictionary :: Dictionary -> IO ()
putDictionary dict 
    | dict == V.empty = putStrLn "No entries found."
    | otherwise       = V.mapM_ (uncurry putDictionaryEntryWithNumber) (V.indexed dict) where
        putDictionaryEntryWithNumber :: Int -> DictionaryEntry -> IO ()
        putDictionaryEntryWithNumber n entry = do
            putStr $ show n ++ "."
            if n >= 10 then putStr " " else putStr "  "
            putStrLn $ "| Japanese: " ++ japanese entry
            putStrLn $ "    | Reading without pitch: " ++ reading entry
            case accent entry of
                Nothing -> putStrLn "    | Accent is not available."
                Just 0 -> putStrLn "    | Word is accentless."
                Just a -> do
                    putStrLn $ "    | Accent appears on mora " ++ show a
                    putStrLn $ "    | Reading with pitch: " ++ (showPronunciation . fromJust . pronunciation $ entry)

------------------------------------------------------------------------------------------------------------------------
