{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module IceAndFire
    ( Book(..)
    , Character(..)
    , House(..)
    , getBookById
    , getBookByName
    , getAllBooks
    , getCharacterById
    , getCharactersByName
    , getCharactersByCulture
    , getCharactersByGender
    , getHouseById
    , getHouseByName
    , getHousesByRegion
    , getHouseByWords
    ) where

import OpenSSL.Session (context)
import Network.Wreq
import Network.HTTP.Client (HttpException)
import Network.HTTP.Client.OpenSSL
import qualified Control.Exception as E
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Char8 (unpack)
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

baseUrl :: String
baseUrl = "https://www.anapioficeandfire.com/api"

-- | Get Book by id number
getBookById :: Int -> IO (Maybe Book)
getBookById = loadSingleById "books"

-- | Get Book by name
getBookByName :: String -> IO [Book]
getBookByName bName = entityQuery "books" [("name", bName)]

-- | Get all books
getAllBooks :: IO [Book]
getAllBooks = entityQuery "books" []

-- | Get Character by id number
getCharacterById :: Int -> IO (Maybe Character)
getCharacterById = loadSingleById "characters"

-- | Get Characters by name
getCharactersByName :: String -> IO [Character]
getCharactersByName cName = entityQuery "characters" [("name", cName)]

-- | Get Character by culture
--
-- Example:
--
-- @
-- 'getCharactersByCulture' \"dothraki\"
-- @
--
-- >>> d <- getCharactersByCulture "dothraki"
-- >>> List.length d
-- 23
getCharactersByCulture :: String -> IO [Character]
getCharactersByCulture cCulture = entityQuery "characters" [("culture", cCulture)]

-- | Get Character by gender
--
-- Example:
--
-- @
-- 'getCharactersByGender' \"female\"
-- @
--
-- >>> f <- getCharactersByGender "female"
-- >>> List.length f
-- 461
getCharactersByGender :: String -> IO [Character]
getCharactersByGender cGender = entityQuery "characters" [("gender", cGender)]

-- | Get House by id number
getHouseById :: Int -> IO (Maybe House)
getHouseById = loadSingleById "houses"

-- | Get House by name
getHouseByName :: String -> IO [House]
getHouseByName hName = entityQuery "houses" [("name", hName)]

-- | Get House by region
--
-- Example:
--
-- @
-- 'getHousesByRegion' \"The Crownlands\"
-- @
--
-- >>> c <- getHousesByRegion "The Crownlands"
-- >>> List.length c
-- 49
getHousesByRegion :: String -> IO [House]
getHousesByRegion hRegion = entityQuery "houses" [("region", hRegion)]

-- | Get a House by its words
getHouseByWords :: String -> IO [House]
getHouseByWords hWords = entityQuery "houses" [("hasWords", "true"), ("words", hWords)]

loadSingleById' :: (FromJSON a) => String -> Int -> IO (Maybe a)
loadSingleById' entityType entityId = withOpenSSL $ do
    let opts = defaults & manager .~ Left (opensslManagerSettings context)
    response <- getWith opts (baseUrl ++ "/" ++ entityType ++ "/" ++ (show entityId) :: String)
    let entityJson = decode (view responseBody response)
    return entityJson

loadSingleById :: (FromJSON a) => String -> Int -> IO (Maybe a)
loadSingleById entityType entityId = (loadSingleById' entityType entityId) `E.catch` handler
        where 
            handler :: HttpException -> IO (Maybe a)
            handler _ = return Nothing

--Mainly used to control common query paramaters like pageSize
--Probably where caching should go
entityQuery :: (FromJSON a) => String -> [(String,String)] -> IO [a]
entityQuery entityType qParams = 
    loadFromQueryUrl (baseUrl ++ "/" ++ entityType ++ "/?pageSize=50" ++ paramsToStr qParams)
        where paramsToStr = foldl (\ acc (k,v) -> acc ++ "&" ++ k ++ "=" ++ v) ""

loadFromQueryUrl :: (FromJSON a) => String -> IO [a]
loadFromQueryUrl url = withOpenSSL $ do
    let opts = defaults & manager .~ Left (opensslManagerSettings context)
    response <- getWith opts url
    let entity = decode (view responseBody response)
    let unpacked = case entity of (Just [])   -> []
                                  (Just list) -> list
                                  Nothing     -> []
    -- Greedily consume and append 'next' links until none more
    let nextLink = (response ^? responseLink "rel" "next" . linkURL)
    let next = case nextLink of (Just linkUrl) -> loadFromQueryUrl (unpack linkUrl)
                                Nothing        -> return ([] :: [a])
    nextList <- next
    return (unpacked ++ nextList)
