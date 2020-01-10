module Downstep where

import Search
import Match
import Text.Read
import qualified Data.Vector as V

data Pronunciation = Mora Char | Downstep

putPronunciation :: [Pronunciation] -> IO ()
putPronunciation [] = putChar '\n'
putPronunciation (x:xs) = do
    case x of
        Mora x' -> putChar x'
        Downstep -> putChar '↓'
    putPronunciation xs

-- FIXME: This is gross
putDictionaryWithPronunciation :: Dictionary -> IO ()
putDictionaryWithPronunciation dict
    | V.length dict == 0 = putStrLn "No entries found."
    | otherwise          = mapM_ (\n -> do 
        putDictionaryEntryWithNumber (n+1) (dict V.! n)
        case applyDownstepToDictionaryEntry (dict V.! n) of
            Left _ -> return ()
            Right pronunciation -> do
                putStr "    | Reading with pitch: "
                putPronunciation pronunciation
        ) [0..(length dict - 1)]

applyDownstep :: Int -> String -> [Pronunciation]
applyDownstep _ "" = []
applyDownstep n xs@(x:xs')
    | n == 0    = map Mora xs
    | n == 1    = Mora x : Downstep : map Mora xs'
    | otherwise = Mora x : applyDownstep (n-1) xs'

applyDownstepToDictionaryEntry :: DictionaryEntry -> Either String [Pronunciation]
applyDownstepToDictionaryEntry entry = let kana = removeGarbage . readingWithoutPitch $ entry in 
    case moraAccent entry of 
        Nothing -> Left "Cannot apply downstep to a dictionary entry with no available accent"
        Just n -> Right $ applyDownstep n kana

    