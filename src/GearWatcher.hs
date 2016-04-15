{-# LANGUAGE OverloadedStrings #-}

module GearWatcher (
  Listing(..),
  newPostings,
) where

import Data.List
import Data.List.Split
import Network.HTTP.Conduit (simpleHttp)
import System.Environment
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor


siteUrl :: String
siteUrl = "http://www.mountainproject.com"

forSaleForum :: String
forSaleForum = siteUrl ++ "/v/for-sale--wanted/103989416"

-- data we are looking for
findNodes :: Cursor -> [Cursor]
findNodes = element "a" >=> attributeIs "target" "_top"

cursorFor :: String -> IO Cursor
cursorFor u = do
  page <- simpleHttp u
  return $ fromDocument $ parseLBS page

-- The strings are coming with strange chracters. Maybe I need a different
-- encoding?
cleanString :: String -> String
cleanString i =
  -- lol wut. Need to do some more learning
  reverse $ init $ init $ reverse $ init $ init i

--------------------------------------------------------------------------------

data Listing = Listing { url :: String
                         , description :: String
                         , postId :: String
                         } deriving (Show)

parseListing :: Cursor -> Listing
parseListing c =
  let u = siteUrl ++ "/v/" ++ (reverse $ takeWhile (/= '/') $ reverse (cleanString (show $ attribute "href" c)))
      d = cleanString $ show $ content $ head $ child c
      pid = reverse $ takeWhile (/= '/') $ reverse u
      in Listing {url = u, description = d, postId = pid}

--------------------------------------------------------------------------------

getFrontPageSales :: Cursor -> [Listing]
getFrontPageSales c =
  map parseListing $ c $// findNodes -- &| extractData

checkedPosts :: String
checkedPosts = "mountainproject.txt"

frontPageSaleIds :: [Listing] -> [String]
frontPageSaleIds = map postId

newPostings :: IO [Listing]
newPostings = do
  cursor <- cursorFor forSaleForum
  checkedPosts <- genEnv "GB_DATA_DIR" ++ checkedPosts
  checkedIds <- readFile checkedPosts

  let frontPageItemsIds = frontPageSaleIds $ getFrontPageSales cursor
  let checkedItemIds = splitOn "\n" checkedIds

  -- list of items noe yet checked yet
  let notYetChecked =frontPageItemsIds \\ checkedItemIds

  let toBeNotifiedOf = filter (\i -> postId i `elem` notYetChecked) $ getFrontPageSales cursor
  mapM_ ((\itm -> appendFile checkedPosts $ itm ++ "\n") . postId) toBeNotifiedOf
  return toBeNotifiedOf
