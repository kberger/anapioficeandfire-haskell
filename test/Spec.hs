{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe
import Test.Hspec
import IceAndFire

main :: IO ()
main = hspec $ do
    describe "getBookById" $ do
        it "returns a book if id is valid" $ do
            book <- getBookById 1
            book `shouldSatisfy` isJust
        it "returns None if id is invalid" $ do
            book <- getBookById 100
            book `shouldSatisfy` isNothing
