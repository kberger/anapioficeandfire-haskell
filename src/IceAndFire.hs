{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module IceAndFire
    ( Book(..)
    , Character(..)
    , House(..)
    , getBookById
    , getCharacterById
    , getCharacterByName
    , getHouseById
    , getHouseByName
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

getBookById :: Int -> IO (Maybe Book)
getBookById = loadSingleById "books"

getCharacterById :: Int -> IO (Maybe Character)
getCharacterById = loadSingleById "characters"

getHouseById :: Int -> IO (Maybe House)
getHouseById = loadSingleById "houses"

loadSingleById :: (FromJSON a) => String -> Int -> IO (Maybe a)
loadSingleById entity id = do
    let url = baseUrl ++ "/" ++ entity ++ "/" ++ (show id) :: String
    response <- get url
    let entity = decode (view responseBody response)
    return entity

getCharacterByName :: String -> IO (Maybe Character)
getCharacterByName name = 
    loadSingleFromQueryUrl (baseUrl ++ "/characters/?name=" ++ name)

getHouseByName :: String -> IO (Maybe House)
getHouseByName name = 
    loadSingleFromQueryUrl (baseUrl ++ "/houses/?name=" ++ name)

loadSingleFromQueryUrl :: (FromJSON a) => String -> IO (Maybe a)
loadSingleFromQueryUrl url = do
    response <- get url
    let entity = decode (view responseBody response)
    let lHead = case entity of (Just [])   -> Nothing
                               (Just list) -> Just (head list)
                               Nothing     -> Nothing
    return lHead
