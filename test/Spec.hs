{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Maybe
import Test.Hspec
import IceAndFire

main :: IO ()
main = hspec $ do
    describe "getBookById" $ do
        it "returns a book if id is valid" $ do
            book <- getBookById 1
            book `shouldSatisfy` isJust
        it "returns Nothing if id is invalid" $ do
            book <- getBookById 100
            book `shouldSatisfy` isNothing

    describe "getCharacterById" $ do
        it "returns a character if id is valid" $ do
            char <- getCharacterById 1
            char `shouldSatisfy` isJust
        it "returns Nothing if id is invalid" $ do
            char <- getCharacterById 1000000
            char `shouldSatisfy` isNothing

    describe "getHouseById" $ do
        it "returns a house if id is valid" $ do
            house <- getHouseById 1
            house `shouldSatisfy` isJust
        it "returns Nothing if id is invalid" $ do
            house <- getHouseById 1000000
            house `shouldSatisfy` isNothing

    describe "getCharactersByName" $ do
        it "returns info on Jon Snow" $ do
            jon <- getCharactersByName "Jon Snow"
            Data.List.length jon `shouldBe` 1
            (charName . Data.List.head) jon `shouldBe` "Jon Snow"
        it "returns empty list if name not found" $ do
            fred <- getCharactersByName "Fred Claus"
            Data.List.length fred `shouldBe` 0
    
    describe "getBookByName" $ do
        it "returns info on A Game of Thrones" $ do
            thrones <- getBookByName "A Game of Thrones"
            Data.List.length thrones `shouldBe` 1
            (bookName . Data.List.head) thrones `shouldBe` "A Game of Thrones"
        it "returns empty list if name not found" $ do
            bible <- getBookByName "The Bible"
            Data.List.length bible `shouldBe` 0

    describe "getHouseByWords" $ do
        it "returns info on house Stark" $ do
            stark <- getHouseByWords "Winter is Coming"
            Data.List.length stark `shouldBe` 1
            (houseName . Data.List.head) stark `shouldBe` "House Stark of Winterfell"
        it "returns an empty list if words not found" $ do
            scrooge <- getHouseByWords "Bah, Humbug!"
            Data.List.length scrooge `shouldBe` 0
