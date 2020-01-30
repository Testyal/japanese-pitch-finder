import qualified Data.ByteString.Lazy as BL
import Criterion.Main
import qualified Data.Vector as V
import Dictionary.Csv
import Dictionary.Search
import Dictionary.Types

sampleEntries :: [String]
sampleEntries = ["映画", "音楽", "雑誌", "朝ご飯", "お酒", "お茶", "水", "家", "読む", "起きる","全然","勉強する"]

benchSingleEntry :: Dictionary -> String -> Benchmark 
benchSingleEntry dict entry = bench ("Dictionary.Search.filterEntries for the entry " ++ entry) $ whnf (filterEntries dict) entry

main :: IO ()
main = do
    wadoku <- BL.readFile "data/NewWaDokuDa.tsv"
    --defaultMain [bench "Dictionary.Csv.convertCsvToDictionary" $ whnf convertCsvToDictionary wadoku]
    putStrLn "Preparing to benchmark the search phase..."
    case convertCsvToDictionary wadoku of
        Left err -> putStrLn err 
        Right dict -> do
            putStrLn "The documentation of the vector package indicates Vector.filter is (not unexpectedly) O(n)."
            putStrLn $ "The WaDoku dictionary has length " ++ show (V.length dict) ++ "."
            defaultMain $ map (benchSingleEntry dict) sampleEntries