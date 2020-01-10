import Expand
import qualified Data.Vector as V
import Match
import Search
import StringUtilities
import Test.Hspec
import qualified Data.ByteString.Lazy as BL

myTest :: Spec
myTest = it "does something" $ 1 == 1 `shouldBe` True 

main :: IO ()
main = do
    hspec . describe "Match" $
        it "expands a given semicolon separated list appropriately" $ parseJapaneseEntry "あ;に;め" `shouldBe` ["あ", "に", "め"]
    hspec . describe "Expand" $
        it "expands a given sample dictionary appropriately" $ expandDictionary myDict `shouldBe` myExpandedDict where
            myDict = V.singleton $ DictionaryEntry "あ; に; め" "foo" (Just 3)
            myExpandedDict = foldr (\s v -> V.cons (DictionaryEntry s "foo" (Just 3)) v) V.empty ["あ","に","め"]