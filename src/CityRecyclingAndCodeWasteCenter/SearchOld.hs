{-# LANGUAGE OverloadedStrings #-}

-- TODO: Split off a Dictionary module
module CityRecyclingAndCodeWasteCenter.SearchOld where

import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Csv
import qualified Data.Vector as V
import CityRecyclingAndCodeWasteCenter.MatchOld
import Text.Read

tsvDecodeOptions :: DecodeOptions
tsvDecodeOptions = defaultDecodeOptions {
    decDelimiter = fromIntegral (ord '\t')
}

data DictionaryEntry = DictionaryEntry {
    japanese :: String,
    readingWithoutPitch :: String,
    moraAccent :: Maybe Int
} deriving (Show, Eq)

instance FromNamedRecord DictionaryEntry where
    parseNamedRecord r = DictionaryEntry <$> r .: "Japanese" 
                                         <*> r .: "Reading" 
                                         <*> (readMaybe <$> r .: "MoraAccent")

instance ToNamedRecord DictionaryEntry where
    toNamedRecord entry = namedRecord ["Japanese" .= japanese entry, "Reading" .= readingWithoutPitch entry, "MoraAccent" .= moraAccent entry]

type Dictionary = V.Vector DictionaryEntry

putDictionaryEntryWithNumber :: Int -> DictionaryEntry -> IO ()
putDictionaryEntryWithNumber n entry = do
    putStr $ show n ++ ". "
    putStrLn $ " | Japanese: " Prelude.++ japanese entry
    putStrLn $ "    | Reading without pitch: " Prelude.++ readingWithoutPitch entry
    putStrLn $ let ma = moraAccent entry in case ma of 
        Nothing -> "    | Accent number not available."
        Just 0 -> "    | Word is accentless"
        Just x -> "    | Accent appears on this mora: " Prelude.++ show x

putDictionaryEntry :: DictionaryEntry -> IO ()
putDictionaryEntry entry = do
    putStrLn $ "| Japanese: " Prelude.++ japanese entry
    putStrLn $ "| Reading without pitch: " Prelude.++ readingWithoutPitch entry
    putStrLn $ let ma = moraAccent entry in case ma of 
        Nothing -> "| Accent number not available."
        Just 0 -> "| Word is accentless"
        Just x -> "| Accent appears on this mora: " Prelude.++ show x

-- FIXME: This is gross
putDictionary :: Dictionary -> IO ()
putDictionary dict
    | V.length dict == 0 = putStrLn "No entries found."
    | otherwise          = mapM_ (\n -> putDictionaryEntryWithNumber (n+1) (dict V.! n)) [0..(length dict - 1)]

{- TODO: Provide multiple options in case the query is matched more than once. 
For example, 私 is mistakenly given the pronunciation あたい since it is the first option encountered in the WaDoku dictionary. -}
findJapaneseEntry :: Dictionary -> String -> Either String DictionaryEntry
findJapaneseEntry entries query = case V.find (\x -> query `match` japanese x) entries of
    Just entry -> Right entry 
    Nothing -> Left "Entry not found."

filterJapaneseEntries :: Dictionary -> String -> Dictionary 
filterJapaneseEntries dict query = V.filter (\entry -> japanese entry == query) dict

convertCsvToDictionary :: BL.ByteString -> Either String Dictionary
convertCsvToDictionary file = case decodeByNameWith tsvDecodeOptions file of 
    Left err -> Left err 
    Right (_, dict) -> Right dict

findJapaneseEntryInCsv :: BL.ByteString -> String -> Either String DictionaryEntry
findJapaneseEntryInCsv csvDict query = do 
    dict <- convertCsvToDictionary csvDict
    findJapaneseEntry dict query