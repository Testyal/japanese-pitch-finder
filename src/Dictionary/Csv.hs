
{-# LANGUAGE OverloadedStrings #-}

module Dictionary.Csv where

import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Csv
import Dictionary.Types
import Text.Read

-- Decode CSV ----------------------------------------------------------------------------------------------------------

tsvDecodeOptions :: DecodeOptions
tsvDecodeOptions = defaultDecodeOptions {
    decDelimiter = fromIntegral (ord '\t')
}

instance FromNamedRecord DictionaryEntry where
    parseNamedRecord r = DictionaryEntry <$> r .: "Japanese" <*> r .: "Reading" <*> (readMaybe <$> r .: "MoraAccent")

{-| Reads a tab separated value file (TSV) with header row "Japanese"   "Reading"   "MoraAccent", returning a Dictionary
whose entries are the rows of this TSV if the conversion is successful, and an error otherwise -}
convertCsvToDictionary :: BL.ByteString -> Either String Dictionary
convertCsvToDictionary csv = snd <$> decodeByNameWith tsvDecodeOptions csv

-- Decoding from a File ------------------------------------------------------------------------------------------------

{-| Like 'convertCsvToDictionary', except it allows reading a TSV from a given filepath -}
readDictionaryFromFilepath :: FilePath -> IO (Either String Dictionary)
readDictionaryFromFilepath path = convertCsvToDictionary <$> BL.readFile path

------------------------------------------------------------------------------------------------------------------------
