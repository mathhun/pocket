{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Pocket.Server.Server (serverMain) where

import Control.Monad
import Control.Monad.Trans
import Data.Aeson (ToJSON)
import Data.Text (Text)
import Database.MySQL.Simple
import Database.MySQL.Simple.Types
import GHC.Generics
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Web.Scotty
import qualified Data.ByteString as B
import qualified Data.Text as T

data ResponseData = ResponseData
  { list :: [PocketData]
  } deriving (Generic)

instance ToJSON ResponseData

data PocketData = PocketData
  { createdAt :: Text
  , status :: Text
  , count :: Int
  } deriving (Generic)

instance ToJSON PocketData

serverMain :: IO ()
serverMain =
  scotty 3000 $ do
    middleware logStdoutDev
    middleware $ staticPolicy $ addBase "static/app"

    get "/" $ do
      text "hello"

    get "/pocket" $ do
      pocket <- liftIO retrievePocket
      json pocket

retrievePocket :: IO ResponseData
retrievePocket = do
  pocket <- selectPocket
  return $ ResponseData { list = pocket }

--
-- MySQL
--

myConnectInfo :: ConnectInfo
myConnectInfo = defaultConnectInfo {
                  connectHost = "127.0.0.1"
                , connectPort = 3306
                , connectUser = "naoto"
                , connectPassword = "naoto"
                , connectDatabase = "pocket"
                }

selectPocket :: IO [PocketData]
selectPocket = do
  conn <- connect myConnectInfo
  rs <- query_ conn queryPocket
  return $ map (\(a, b, c) -> PocketData a b c) rs

queryPocket :: Query
queryPocket = Query $ B.concat
    [ "select created_at, case when favorite = 1 then 'favorite'"
    ,  "          when time_read > '1970-01-01 00:00' then 'keep'"
    ,  "          when created_at = time_added then 'added'"
    ,  "          when created_at = time_read then 'read'"
    ,  "          when created_at = time_favorited then 'favorited'"
    ,  "          else 'unread' end status"
    ,  "   , count(*) count"
    ,  " from ("
    ,  "   select item_id"
    ,  "     , favorite"
    ,  "     , date_format(created_at, '%Y-%m-%d %H:00') created_at"
    ,  "     , date_format(time_added, '%Y-%m-%d %H:00') time_added"
    ,  "     , date_format(time_read, '%Y-%m-%d %H:00') time_read"
    ,  "     , date_format(time_favorited, '%Y-%m-%d %H:00') time_favorited"
    ,  "   from article"
    ,  "   where created_at >= date_add(now(), interval -10 day)"
    ,  " ) t"
    ,  " group by status, created_at"
    ]

