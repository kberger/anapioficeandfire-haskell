{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Lib
    ( someFunc,
      Book
    ) where

import Network.Wreq
import GHC.Generics
import Data.Aeson
import Data.List

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Book = Book
    { url :: String
    , name :: String
    , isbn :: String
    , authors :: [String]
    , numberOfPages :: Integer
    , publisher :: String
    , country :: String
    , mediaType :: String
    , released :: String
    , characters :: [String]
    , povCharacters :: [String] 
    } deriving (Generic, Show)

instance FromJSON Book


