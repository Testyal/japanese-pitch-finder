module TeXGenerator where

import Downstep
import Search

generateTeXFromPronunciation :: [Pronunciation] -> String 
generateTeXFromPronunciation [] = ""
generateTeXFromPronunciation (x:xs) = case x of 
    Mora m -> m : generateTeXFromPronunciation xs
    Downstep -> "\\smash{\\textdownstep}" ++ generateTeXFromPronunciation xs

generateTeXDictionaryEntry :: String -> [Pronunciation] -> String 
generateTeXDictionaryEntry word pronunciation = "\\dictentry{" ++ word ++ "}{}{" ++ generateTeXFromPronunciation pronunciation ++ "}{}{}"