
module Dictionary.Types where

import Data.Maybe
import qualified Data.Vector as V

-- Pronunciation --------------------------------------------------------------------------------------------------

{-| Phonemes for the purpose of this module are any kana (including the small sokuon and yōon characters) and 
a phoneme representing a downstep in pitch accent. Each is treated specially since yōon don't count as morae,
and I barely understand the sokuon so I might need to change some stuff about it in the future when I learn more
about how pitch accent works. -}
data Phoneme = Mora Char | Downstep | Sokuon | Yoon Char

{-| Given a phoneme, displays a character representing it. Downstep in particular is represented by a down arrow. -}
showPhoneme :: Phoneme -> Char
showPhoneme (Mora c) = c
showPhoneme (Yoon c) = c
showPhoneme Sokuon = 'っ'
showPhoneme Downstep = '↓'

{-| Essentially the reverse of 'showPhoneme', but there is no character to construct a Downstep. -}
constructPhoneme :: Char -> Phoneme 
constructPhoneme c
    | c `elem` ['ゃ', 'ょ', 'ゅ'] = Yoon c
    | c == 'っ'                  = Sokuon
    | otherwise                  = Mora c

type Pronunciation = [Phoneme]

{-| AccentlessPronunciation exists to encourage somewhat more correct code. Objects of this type should not 
include any Downsteps. -}
type AccentlessPronunciation = Pronunciation

{-| Given a sequence of kana, constructs its Pronunciation without Downsteps. -}
constructAccentlessPronunciation :: String -> AccentlessPronunciation
constructAccentlessPronunciation = map constructPhoneme

{-| Given a pronunciation, displays a sequence of kana representing it. -}
showPronunciation :: Pronunciation -> String 
showPronunciation = map showPhoneme

{-| Inserts a Downstep into an AccentlessPronunciation at the given mora. A mora is any of the phonemes Mora
c or Sokuon, but not Yoon c. Inserting a Downstep at position 0 does nothing, due to how data is stored 
in the table. Namely, accentless Japanese words are represented with an accent of 0. -}
applyDownstepToPronunciation :: Int -> AccentlessPronunciation -> Pronunciation
applyDownstepToPronunciation 0 ps = ps
applyDownstepToPronunciation 1 (p:ps) = p : applyDownstepImmediately ps where
    applyDownstepImmediately (Yoon c : ps) = Yoon c : Downstep : ps 
    applyDownstepImmediately ps = Downstep : ps
-- applyDownstepToPronunciation n (Sokuon:ps) = Sokuon : applyDownstepToPronunciation n ps
applyDownstepToPronunciation n (p:ps) = p : applyDownstepToPronunciation (n-1) ps

{-| Like 'applyDownstepToPronunciation', but first constructs the AccentlessPronunciation for a given sequence
of kana. -}
applyDownstepToReading :: Int -> String -> Pronunciation
applyDownstepToReading n = applyDownstepToPronunciation n . constructAccentlessPronunciation

{-# DEPRECATED applyDownstepToMora "use applyDownstepToReading instead" #-}
applyDownstepToMora :: Int -> String -> Pronunciation
applyDownstepToMora 0 ms = map Mora ms
applyDownstepToMora 1 (m:ms) = Mora m : Downstep : map Mora ms
applyDownstepToMora n (m:ms) = Mora m : applyDownstepToMora (n-1) ms

-- DictionaryEntry ------------------------------------------------------------------------------------------------

data DictionaryEntry = DictionaryEntry {
    japanese :: String,
    reading :: String,
    accent :: Maybe Int
} deriving (Eq)

pronunciation :: DictionaryEntry -> Maybe Pronunciation
pronunciation entry = case accent entry of 
    Nothing -> Nothing 
    Just a -> Just $ applyDownstepToReading a (reading entry)

-- Dictionary -----------------------------------------------------------------------------------------------------

type Dictionary = V.Vector DictionaryEntry

empty :: Dictionary
empty = V.empty

singleton :: DictionaryEntry -> Dictionary
singleton = V.singleton

(!) :: Dictionary -> Int -> DictionaryEntry
(!) = (V.!)

-------------------------------------------------------------------------------------------------------------------
