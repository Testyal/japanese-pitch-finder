-- TODO: Split off a Parser module to absorb the effects of parseJapaneseEntry
module CityRecyclingAndCodeWasteCenter.MatchOld where

import CityRecyclingAndCodeWasteCenter.StringUtilitiesOld

-- TODO: Implement something a lot, lot faster. Potentially could be improved a lot by reserializing the data so we can perform a search algorithm
match :: String -> String -> Bool
match xs ys = xs `elem` parseJapaneseEntry ys

parseJapaneseEntry :: String -> [String]
{- This naive (and trash) implementation is to replace all occurences of a left parenthesis with a semicolon, 
thenremove all whitespace and all characters which aren't kana/kanji or semicolons, and finally break
the string at semicolons. This leaves the issue of rōmaji in words such as Tシャツ, which will be
replaced by the potentially erroneous シャツ. Luckily, the table seems to contain the katakana
alternatives to rōmaji, so during the search phase, the program could replace all instances of
rōmaji with katakana, then search with that. -}
parseJapaneseEntry = splitAtSemicolons . removeGarbage . replaceLeftParen

garbage :: String
garbage = " \t\nabcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ[]1234567890),"

removeGarbage :: String -> String
removeGarbage "" = ""
removeGarbage (x:xs)
    | x `elem` garbage = removeGarbage xs
    | otherwise        = x : removeGarbage xs

replaceLeftParen :: String -> String
replaceLeftParen "" = ""
replaceLeftParen (x:xs)
    | x == '('   = ';' : replaceLeftParen xs
    | otherwise  = x : replaceLeftParen xs

