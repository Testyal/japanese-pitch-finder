{-# LANGUAGE OverloadedStrings #-}

module Serialize where

import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Csv
import Expand
import Search

main :: IO ()
main = do
    putStrLn "Reformatting dictionary..."
    wadoku <- BL.readFile "data/WaDokuDa.tsv"
    case convertCsvToDictionary wadoku of 
        Left err -> putStrLn err 
        Right dict -> BL.writeFile "data/NewWaDokuDa.tsv" reformattedDict where
            reformattedDict = encodeJapaneseDictToCsv . expandDictionary $ dict
    putStrLn "Done."

encodeJapaneseDictToCsv :: Dictionary -> BL.ByteString
encodeJapaneseDictToCsv dict = encodeByNameWith tsvEncodeOptions (header ["Japanese", "Reading", "MoraAccent"]) (V.toList dict)

tsvEncodeOptions = defaultEncodeOptions {
    encDelimiter = fromIntegral (ord '\t')
}