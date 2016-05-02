{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Lib
    ( someFunc,
      Book,
      Character,
      House
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

data Character = Character
    { url :: String
    , name :: String
    , gender :: String
    , culture :: String
    , born :: String
    , died :: String
    , titles :: [String]
    , aliases :: [String]
    , father :: String
    , mother :: String
    , spouse :: String
    , allegiances :: [String]
    , books :: [String]
    , povBooks :: [String]
    , tvSeries :: [String]
    , playedBy :: [String]
    } deriving (Generic, Show)

instance FromJSON Character

data House = House
    { url :: String
    , name :: String
    , region :: String
    , coatOfArms :: String
    , words :: String
    , titles :: [String]
    , seats :: [String]
    , currentLord :: String
    , heir :: String
    , overlord :: String
    , founded :: String
    , founder :: String
    , diedOut :: String
    , ancestralWeapons :: [String]
    , cadetBranches :: [String]
    , swornMembers :: [String]
    } deriving (Generic, Show)

instance FromJSON House
