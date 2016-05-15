{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module IceAndFire
    ( Book(..)
    , Character(..)
    , House(..)
    , loadBook
    , loadCharacter
    , loadHouse
    ) where

import Network.Wreq
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
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
    } deriving Show

instance FromJSON Book where
    parseJSON (Object v) = Book <$>
                           v .: "url" <*>
                           v .: "name" <*>
                           v .: "isbn" <*>
                           v .: "authors" <*>
                           v .: "numberOfPages" <*>
                           v .: "publisher" <*>
                           v .: "country" <*>
                           v .: "mediaType" <*>
                           v .: "released" <*>
                           v .: "characters" <*>
                           v .: "povCharacters"
    parseJSON invalid    = typeMismatch "Book" invalid

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
    } deriving Show

instance FromJSON Character where
    parseJSON (Object v) = Character <$>
                           v .: "url" <*>
                           v .: "name" <*>
                           v .: "gender" <*>
                           v .: "culture" <*>
                           v .: "born" <*>
                           v .: "died" <*>
                           v .: "titles" <*>
                           v .: "aliases" <*>
                           v .: "father" <*>
                           v .: "mother" <*>
                           v .: "spouse" <*>
                           v .: "allegiances" <*>
                           v .: "books" <*>
                           v .: "povBooks" <*>
                           v .: "tvSeries" <*>
                           v .: "playedBy"
    parseJSON invalid    = typeMismatch "Character" invalid

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
    } deriving Show

instance FromJSON House where
    parseJSON (Object v) = House <$>
                           v .: "url" <*>
                           v .: "name" <*>
                           v .: "region" <*>
                           v .: "coatOfArms" <*>
                           v .: "words" <*>
                           v .: "titles" <*>
                           v .: "seats" <*>
                           v .: "currentLord" <*>
                           v .: "heir" <*>
                           v .: "overlord" <*>
                           v .: "founded" <*>
                           v .: "founder" <*>
                           v .: "diedOut" <*>
                           v .: "ancestralWeapons" <*>
                           v .: "cadetBranches" <*>
                           v .: "swornMembers"
    parseJSON invalid    = typeMismatch "House" invalid

baseUrl = "http://www.anapioficeandfire.com/api"

loadBook :: Int -> IO (Maybe Book)
loadBook = loadSingleById "books"

loadCharacter :: Int -> IO (Maybe Character)
loadCharacter = loadSingleById "characters"

loadHouse :: Int -> IO (Maybe House)
loadHouse = loadSingleById "houses"

loadSingleById :: (FromJSON a) => String -> Int -> IO (Maybe a)
loadSingleById entity id = do
    let url = baseUrl ++ "/" ++ entity ++ "/" ++ (show id) :: String
    response <- get url
    let entity = decode (view responseBody response)
    return entity
