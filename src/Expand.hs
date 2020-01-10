module Expand where

import Control.Monad
import qualified Data.Vector as V
import Match
import Search

data BizarroDictionaryEntry = BizarroDictionaryEntry {
    japanese :: [String],
    reading :: String,
    moraAccent :: Maybe Int
}

type BizarroDictionary = V.Vector BizarroDictionaryEntry

parseDictionaryEntry :: DictionaryEntry -> BizarroDictionaryEntry
parseDictionaryEntry (DictionaryEntry japanese x y) = BizarroDictionaryEntry (parseJapaneseEntry japanese) x y

splitBizarro :: BizarroDictionaryEntry -> V.Vector DictionaryEntry
splitBizarro (BizarroDictionaryEntry [] x y) = V.empty 
splitBizarro (BizarroDictionaryEntry (j:js) x y) = V.cons (DictionaryEntry j x y) $ splitBizarro (BizarroDictionaryEntry js x y)

expandDictionary :: Dictionary -> Dictionary
expandDictionary = join . V.map (splitBizarro . parseDictionaryEntry)