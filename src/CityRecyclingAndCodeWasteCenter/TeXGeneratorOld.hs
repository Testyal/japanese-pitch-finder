module CityRecyclingAndCodeWasteCenter.TeXGeneratorOld where

import CityRecyclingAndCodeWasteCenter.DownstepOld
import CityRecyclingAndCodeWasteCenter.SearchOld

generateTeXFromPronunciation :: [Pronunciation] -> String 
generateTeXFromPronunciation [] = ""
generateTeXFromPronunciation (x:xs) = case x of 
    Mora m -> m : generateTeXFromPronunciation xs
    Downstep -> "\\smash{\\textdownstep}" ++ generateTeXFromPronunciation xs

generateTeXDictionaryEntry :: String -> [Pronunciation] -> String 
generateTeXDictionaryEntry word pronunciation = "\\dictentry{" ++ word ++ "}{}{" ++ generateTeXFromPronunciation pronunciation ++ "}{}{}"