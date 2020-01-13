module CityRecyclingAndCodeWasteCenter.StringUtilitiesOld where

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen _ [] = []
splitWhen p xs = let (ys, ys') = breakWhen p xs in 
    mergeIfNonempty ys $ splitWhen p ys' where
        mergeIfNonempty :: [a] -> [[a]] -> [[a]]
        mergeIfNonempty [] xss = xss
        mergeIfNonempty ys xss = ys : xss

splitLines :: String -> [String]
splitLines = splitWhen (== '\n')

splitAtTabs :: String -> [String]
splitAtTabs = splitWhen (== '\t')

splitAtSemicolons :: String -> [String]
splitAtSemicolons = splitWhen (== ';')

breakWhen :: (a -> Bool) -> [a] -> ([a], [a])
breakWhen _ [] = ([],[])
breakWhen p xs@(x:xs')
    | p x       = ([], xs')
    | otherwise = let (ys, ys') = breakWhen p xs' in (x:ys, ys')

