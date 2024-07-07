{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Data.ByteString.Lazy.Char8 (unpack)
import Data.Default (def)
import Data.List (isSuffixOf)
import Data.Maybe (fromJust)
import Network.HTTP.Client (
    ManagerSettings,
    managerModifyRequest,
    httpLbs,
    newManager,
    parseRequest,
    responseBody,
    requestHeaders)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (hUserAgent)
import System.Directory (removeFile)
import System.FilePath ((</>), takeFileName)
import System.FilePattern.Directory (getDirectoryFiles)
import Text.HTML.Scalpel ((@:), Scraper, URL, attr, chroots, hasClass, manager, scrapeURLWithConfig)

cleanAddedTorrents :: IO ()
cleanAddedTorrents = do alreadyAddedFiles <- getDirectoryFiles "/home/chapman/Downloads" ["*.added"]
                        mapM_ (\fileName -> removeFile ("/home/chapman/Downloads/" ++ fileName)) alreadyAddedFiles

-- Create a new manager settings based on the default TLS manager that updates
-- the request headers to include a custom user agent.
managerSettings :: ManagerSettings
managerSettings = tlsManagerSettings {
  managerModifyRequest = \req -> do
    req' <- managerModifyRequest tlsManagerSettings req
    return $ req' {
      requestHeaders = (hUserAgent, "update-linux-torrents")
                          : requestHeaders req'
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
mostRecentTorrents = do manager <- Just <$> newManager managerSettings
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
