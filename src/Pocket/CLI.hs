{-# LANGUAGE DeriveGeneric, OverloadedStrings, FlexibleInstances, ScopedTypeVariables #-}
module Pocket.CLI (pocketMain) where

import Control.Applicative ((<$>), (<*>))
import Control.Lens hiding (Action)
import Data.Aeson
import Data.Map as Map hiding (map)
import Data.Text hiding (map)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Time.LocalTime
import Database.MySQL.Simple
import Database.MySQL.Simple.Param
import Database.MySQL.Simple.QueryParams
import GHC.Generics (Generic)
import Network.Wreq
import System.Locale

import Pocket.Credentials (consumerKey, accessToken)

instance FromJSON JSONResponse
instance FromJSON JSONEntry where
  parseJSON (Object v) = JSONEntry
                         <$> v .: "item_id"
                         <*> v .: "resolved_id"
                         <*> v .: "given_url"
                         <*> v .: "given_title"
                         <*> v .: "favorite"
                         <*> v .: "status"
                         <*> v .: "time_added"
                         <*> v .: "time_updated"
                         <*> v .: "time_read"
                         <*> v .: "time_favorited"
                         <*> v .: "sort_id"
                         <*> v .: "resolved_title"
                         <*> v .: "resolved_url"
                         <*> v .: "excerpt"
                         <*> v .: "is_article"
                         <*> v .: "is_index"
                         <*> v .: "has_video"
                         <*> v .: "has_image"
                         <*> v .: "word_count"

type Resp = Response (Map String Value)

data JSONResponse = JSONResponse {
                      status :: Int
                    , complete :: Int
                    , list :: Map Text JSONEntry
                    , error :: Maybe Text
                    , search_meta :: Map Text Text
                    , since :: Int
                    } deriving (Show, Generic)

data JSONEntry = JSONEntry {
                   itemId :: Text
                 , resolvedId :: Text
                 , givenUrl :: Text
                 , givenTitle :: Text
                 , favorite :: Text
                 , itemStatus :: Text
                 , timeAdded :: Text
                 , timeUpdated :: Text
                 , timeRead :: Text
                 , timeFavorited :: Text
                 , sortId :: Int
                 , resolvedTitle :: Text
                 , resolvedUrl :: Text
                 , excerpt :: Text
                 , isArticle :: Text
                 , isIndex :: Text
                 , hasVideo :: Text
                 , hasImage :: Text
                 , wordCount :: Text
                 } deriving (Show, Generic)

getPocket :: IO [JSONEntry]
getPocket = do
  let opts = defaults & param "consumer_key" .~ [consumerKey] & param "access_token" .~ [accessToken]
  (r :: Response JSONResponse) <- asJSON =<< getWith opts "https://getpocket.com/v3/get"
  return [ value | (_, value) <- toList (list (r ^. responseBody)) ]

myConnectInfo :: ConnectInfo
myConnectInfo = defaultConnectInfo {
                  connectHost = "127.0.0.1"
                , connectPort = 3306
                , connectUser = "naoto"
                , connectPassword = "naoto"
                , connectDatabase = "pocket"
                }

insertToDB :: (QueryParams q) => [q] -> IO ()
insertToDB queryParams = do
  conn <- connect myConnectInfo
  executeMany conn "insert into article (created_at, item_id, resolved_id, given_url, given_title, favorite, status, time_added, time_updated, time_read, time_favorited, sort_id, resolved_title, resolved_url, excerpt, is_article, is_index, has_video, has_image, word_count) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)" queryParams
  return ()

toSql :: JSONEntry -> [Action]
toSql e = map render [ itemId e, resolvedId e, givenUrl e, givenTitle e, favorite e, itemStatus e
                     , intToDate' (timeAdded e), intToDate' (timeUpdated e)
                     , intToDate' (timeRead e), intToDate' (timeFavorited e)
                     , pack (show (sortId e))
                     , resolvedTitle e, resolvedUrl e
                     , excerpt e, isArticle e, isIndex e, hasVideo e, hasImage e, wordCount e
                     ]

intToDate :: Int -> String
intToDate ut = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" $ posixSecondsToUTCTime $ fromIntegral ut

intToDate' :: Text -> Text
intToDate' t = pack $ intToDate $ read $ unpack t

getCurrentTime' :: IO Text
getCurrentTime' = do
  utcTime <- getCurrentTime
  return $ pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:00:00" utcTime

pocketMain :: IO ()
pocketMain = do
  time <- getCurrentTime'
  entries <- getPocket
  print entries
  insertToDB $ map (render time :) $ map toSql entries


--------------------------------------------------------------------------------
-- test
--

sampleJson :: JSONEntry
sampleJson = JSONEntry {
               itemId          = "123"
             , resolvedId      = "123"
             , givenUrl        = "http://url/url"
             , givenTitle      = "my title"
             , favorite        = "1"
             , itemStatus      = "1"
             , timeAdded       = "1407337936"
             , timeUpdated     = "1407337936"
             , timeRead        = "1407337936"
             , timeFavorited   = "1407337936"
             , sortId          = 1
             , resolvedTitle   = "my title"
             , resolvedUrl     = "http://url/url"
             , excerpt         = "aaaaa"
             , isArticle       = "1"
             , isIndex         = "1"
             , hasVideo        = "1"
             , hasImage        = "1"
             , wordCount       = "123"
             }
