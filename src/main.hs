{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, NoMonomorphismRestriction #-}

import Database.MongoDB as M
import Control.Monad.Trans (liftIO)
import System.Locale
import Data.Time
import Data.Time.Format
import Data.Bson.Json
import Data.Aeson.Encode
import Data.Aeson
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Web.Scotty
import Data.Maybe
import Control.Monad.IO.Class
import Network.HTTP.Types (status200,status404)


doMongo :: MonadIO m => Database -> M.Action m a -> m a
doMongo db action = do
  pipe <- liftIO $ runIOE $ connect (host "127.0.0.1")
  result <- access pipe master db action
  liftIO $ close pipe
  case result of
    Right val -> return val 
    Left failure -> fail $ show failure

insertFeeding :: Document -> M.Action IO M.Value
insertFeeding b = insert "breastfeeding" $ exclude ["_id"] feed --we should remove any _id
              where
                feed = merge ["date" =: (dateFromString stringDate), "time" =: (timeFromString stringTime)] b
                stringDate = M.at "date" b :: String
                stringTime = M.at "time" b :: String

updateFeeding :: Document -> M.Action IO () 
updateFeeding b = save "breastfeeding" feed
              where
                feed = merge ["date" =: (dateFromString stringDate), "time" =: (timeFromString stringTime)] b
                stringDate = M.at "date" b :: String
                stringTime = M.at "time" b :: String

deleteFeeding :: ObjectId -> M.Action IO ()
deleteFeeding id = deleteOne $ select ["_id" =: id] "breastfeeding"

breastFeedingById :: ObjectId -> M.Action IO (Maybe Document)
breastFeedingById key = do
  findOneBreastFeeding key

findOneBreastFeeding id = findOne (select ["_id" =: id] "breastfeeding")

allBreastFeedings :: M.Action IO [Document]
allBreastFeedings = rest =<< find (select [] "breastfeeding") {sort = ["date" =: -1, "time" =: -1]} {limit = 10}

dateFromString :: String -> UTCTime
dateFromString s = readTime defaultTimeLocale "%Y-%m-%d" s :: UTCTime

--parse time in the form of xx:xx to a UTCTime
timeFromString :: String -> UTCTime
timeFromString s = readTime defaultTimeLocale "%H:%M" s :: UTCTime

main :: IO ()
main = scotty 3000 $ do
    middleware logStdout
    middleware $ staticPolicy (noDots >-> addBase "static")

    get "/" $ do
        redirect "/index.html"

    get "/api/feedings" $ do
      e <- liftIO $ doMongo "breastfeeding" allBreastFeedings 
      Web.Scotty.json e

    get "/api/feedings/:objectId" $ do
      key <- param "objectId" :: ActionM String 
      e <- liftIO $ doMongo "breastfeeding" (breastFeedingById (read key))
      case e of
            Just p -> Web.Scotty.json p
            Nothing -> status status404

    post "/api/feedings" $ do
        v <- jsonData 
        val <- liftIO $ doMongo "breastfeeding" (insertFeeding v)
        let oid = (read $ show val) :: ObjectId
        result <- liftIO $ doMongo "breastfeeding" (breastFeedingById oid)
        Web.Scotty.json $ result

    put "/api/feedings/:objectId" $ do
        id <- param "objectId" :: ActionM String
        v <- jsonData
        let objectid = read id :: ObjectId  
        result <- liftIO $ doMongo "breastfeeding" (updateFeeding (merge ["_id" =: objectid] v))
        Web.Scotty.json result
  
    Web.Scotty.delete "/api/feedings/:objectId" $ do
        id <- param "objectId" :: ActionM String
        let objectid = read id :: ObjectId
        liftIO $ doMongo "breastfeeding" (deleteFeeding objectid)
        status status200
