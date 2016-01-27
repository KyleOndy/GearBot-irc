{-# LANGUAGE OverloadedStrings #-}

module GearWatcher (
  Listing(..),
  getFrontPageListings,
) where

import Data.Char
import Data.List (isInfixOf)
import Network.HTTP.Conduit (simpleHttp)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor

data Listing = Listing { url :: String
                         , description :: String
                         , postId :: String
                         } deriving (Show, Eq)

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

parseListing :: Cursor -> Listing
parseListing c =
  let u = siteUrl ++ cleanString (show $ attribute "href" c)
      d = cleanString $ show $ content $ head $ child c
      pid = reverse $ takeWhile (/= '/') $ reverse u
      in Listing {url = u, description = d, postId = pid}

--------------------------------------------------------------------------------

getFrontPageSales :: Cursor -> [Listing]
getFrontPageSales c =
  map parseListing $ c $// findNodes -- &| extractData

ignoreSold :: [Listing] -> [Listing]
ignoreSold l =
  filter (\itm -> isInfixOf "sold" (map toLower $ description itm))  l

--------------------------------------------------------------------------------

getFrontPageListings :: IO [Listing]
getFrontPageListings = do
  cursor <- cursorFor forSaleForum
  return $ ignoreSold $ getFrontPageSales cursor
