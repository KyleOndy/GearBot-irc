{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.List
import Network
import System.IO
import Control.Monad
import System.Environment
import Text.Printf
import Control.Concurrent
import GearWatcher

server :: String
server = "irc.snoonet.org"

port :: Integer
port = 6667

chan :: String
chan = "#climbinggear"

nick :: String
nick = "gearbot"

main :: IO ()
main = do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    -- join the server
    forkIO $ do
      threadDelay(microSecondsPerSeconds * 5)
      write h "NICK" nick
      write h "USER" (nick++" 0 * :gearbot")
      threadDelay(microSecondsPerSeconds * 5)
      password <- getEnv "gearbotpass"
      write h "PRIVMSG NickServ" (":identify gearbot " ++ password)
      write h "JOIN" chan
      -- print welcome message
      postListings h
    listen h

microSecondsPerSeconds :: Int
microSecondsPerSeconds = 1000000

formatListing :: Listing -> String
formatListing l = description l ++ " - " ++ url l

postListings :: Handle -> IO ()
postListings h = do
      l <- newPostings
      unless (null l) (mapM_ (privmsg h . formatListing) l)
      threadDelay(microSecondsPerSeconds * 60)
      postListings h


write :: Handle -> String -> String -> IO ()
write h s t = do
    hPrintf h "%s %s\r\n" s t
    printf    "> %s %s\n" s t

listen :: Handle -> IO ()
listen h = loop $ do
    t <- hGetLine h
    let s = init t
    when (ping s) (pong s)
    putStrLn s
  where
    loop a = a >> loop a
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write h "PONG" (':' : drop 6 x)

privmsg :: Handle -> String -> IO ()
privmsg h s = write h "PRIVMSG" (chan ++ " :" ++ s)
