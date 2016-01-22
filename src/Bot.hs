{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.List
import Network
import System.IO
import System.Exit
import Text.Printf
import Control.Concurrent
import GearWatcher

server :: String
server = "irc.snoonet.org"

port :: Integer
port = 6667

chan :: String
chan = "#climbing"

nick :: String
nick = "GearBot"

main :: IO ()
main = do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    -- join the server
    write h "NICK" nick
    write h "USER" (nick++" 0 * :tutorial bot")
    write h "JOIN" chan
    -- print welcome message
    forkIO $ do
      {-
      to prevent sending all the front page deals when we start a new bot
      pass a list of current items on the front page so we only alert on 
      new posts.

      Right now if a post is bumped it will alert like it is a new post since
      it will appear on the front page for the first time
      -}
      initalFrontPage <- getFrontPageListings
      watchForNewGear h initalFrontPage
    listen h

microSecondsPerSeconds :: Int
microSecondsPerSeconds = 1000000

watchForNewGear :: Handle -> [Listing] -> IO ()
watchForNewGear h l = do
      currentFrontPageListings <-getFrontPageListings
      let newListings = currentFrontPageListings \\ l
      if length newListings > 0
      then do
        let newestUnalertedListing = head $ reverse newListings
        privmsg h $ (formatListing newestUnalertedListing)
        -- there may be more gear to check. Quickly!
        threadDelay(microSecondsPerSeconds * 5)
        watchForNewGear h (l ++ [newestUnalertedListing])
      else do
        -- no gear at the moment. Yawn.
        threadDelay(microSecondsPerSeconds * 60)
        watchForNewGear h l

formatListing :: Listing -> String
formatListing l = description l ++ " - " ++ url l


write :: Handle -> String -> String -> IO ()
write h s t = do
    hPrintf h "%s %s\r\n" s t
    printf    "> %s %s\n" s t

listen :: Handle -> IO ()
listen h = forever $ do
    t <- hGetLine h
    let s = init t
    if ping s then pong s else eval h (clean s)
    putStrLn s
  where
    forever a = a >> forever a
    clean     = reverse . takeWhile(/= ':') . reverse
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write h "PONG" (':' : drop 6 x)

eval :: Handle -> String -> IO ()
eval h  "gb!quit" = write h "QUIT" ":Exiting" >> exitWith ExitSuccess
eval h  "gb!info" = printInfo h
eval _   _        = return () -- ignore everything else

privmsg :: Handle -> String -> IO ()
privmsg h s = write h "PRIVMSG" (chan ++ " :" ++ s)

printInfo ::Handle ->  IO ()
printInfo h = do
    privmsg h "Hello!. I am GearBot v[version]"
    privmsg h "I troll the internet and post deals on gear you want"
    privmsg h "Hack on me at https://github.com/KyleOndy/GearBot-irc"
    privmsg h "If I am doing bad things just type 'gb!quit' to make me go away"
