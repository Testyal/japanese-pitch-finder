import qualified Data.ByteString.Lazy as BL
import Criterion.Main
import CityRecyclingAndCodeWasteCenter.MatchOld
import CityRecyclingAndCodeWasteCenter.SearchOld

main :: IO ()
main = do
    dict <- BL.readFile "data/WaDokuDa.tsv"
    defaultMain [bench "convertCsvToDictionary" $ whnf convertCsvToDictionary dict]
    putStrLn "very fast :)"