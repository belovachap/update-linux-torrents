{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Lib

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>), takeFileName)
import Data.List (isSuffixOf)
import Data.Maybe (fromJust)
import Data.Default (def)
import System.Directory
import System.FilePattern.Directory
import Text.HTML.Scalpel
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import Data.ByteString.Lazy.Char8 (unpack)

cleanAddedTorrents :: IO ()
cleanAddedTorrents = do alreadyAddedFiles <- getDirectoryFiles "/home/chapman/Downloads" ["*.added"]
                        mapM_ (\fileName -> removeFile ("/home/chapman/Downloads/" ++ fileName)) alreadyAddedFiles

-- Create a new manager settings based on the default TLS manager that updates
-- the request headers to include a custom user agent.
managerSettings :: HTTP.ManagerSettings
managerSettings = HTTP.tlsManagerSettings {
  HTTP.managerModifyRequest = \req -> do
    req' <- HTTP.managerModifyRequest HTTP.tlsManagerSettings req
    return $ req' {
      HTTP.requestHeaders = (HTTP.hUserAgent, "update-linux-torrents")
                          : HTTP.requestHeaders req'
    }
}

downloadRecentTorrents :: IO ()
downloadRecentTorrents = do torrentURLs <- mostRecentTorrents
                            let recent = take 25 $ filter (\x -> ".torrent" `isSuffixOf` x) (fromJust torrentURLs)
                            mapM_ (\x -> do let url = "https://distrowatch.com/" ++ x -- replace with the actual URL
                                            manager <- newManager managerSettings

                                            request <- parseRequest url
                                            response <- httpLbs request manager

                                            let filePath = "/home/chapman/Downloads" </> (takeFileName x)

                                            writeFile filePath $ unpack (responseBody response))
                                  recent

mostRecentTorrents :: IO (Maybe [URL])
mostRecentTorrents = do manager <- Just <$> HTTP.newManager managerSettings
                        scrapeURLWithConfig (def { manager }) "https://distrowatch.com/dwres.php?resource=bittorrent" torrentURLs
    where
        torrentURLs :: Scraper String [URL]
        torrentURLs = chroots ("td" @: [hasClass "torrent"]) torrentURL

        torrentURL :: Scraper String URL
        torrentURL = do url <- attr "href" "a"
                        return url

main :: IO ()
main = do cleanAddedTorrents
          downloadRecentTorrents
