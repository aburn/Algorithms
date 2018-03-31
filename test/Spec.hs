import           Lib
import           Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Stock Span" $ do
        it "function is same" $ do
            stockSpanQuad [100, 80, 60, 70, 60, 75, 85] `shouldBe` stockSpanLinear [100, 80, 60, 70, 60, 75, 85]
