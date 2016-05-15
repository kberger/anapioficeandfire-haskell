{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module IceAndFire
    ( Book,
      Character,
      House
    ) where

import Network.Wreq
import GHC.Generics
import Data.Aeson
import Data.List
import Control.Lens

data Book = Book
    { bookUrl :: String
    , bookName :: String
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
    { charUrl :: String
    , charName :: String
    , gender :: String
    , culture :: String
    , born :: String
    , died :: String
    , charTitles :: [String]
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
    { houseUrl :: String
    , houseName :: String
    , region :: String
    , coatOfArms :: String
    , words :: String
    , houseTitles :: [String]
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
