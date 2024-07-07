{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString.Lazy.Char8 (unpack)
import Data.Default (def)
import Data.List (isSuffixOf)
import Data.Maybe (fromJust)
import Network.HTTP.Client
  ( ManagerSettings,
    httpLbs,
    managerModifyRequest,
    newManager,
    parseRequest,
    requestHeaders,
    responseBody,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (hUserAgent)
import System.Directory (removeFile)
import System.FilePath (takeFileName, (</>))
import System.FilePattern.Directory (getDirectoryFiles)
import Text.HTML.Scalpel (Scraper, URL, attr, chroots, hasClass, manager, scrapeURLWithConfig, (@:))

downloads :: FilePath
downloads = "/home/chapman/Downloads"

distroWatch :: URL
distroWatch = "https://distrowatch.com"

cleanAddedTorrents :: IO ()
cleanAddedTorrents = do
  alreadyAddedFiles <- getDirectoryFiles downloads ["*.added"]
  mapM_ (\x -> removeFile (downloads </> x)) alreadyAddedFiles

addUserAgent :: ManagerSettings
addUserAgent =
  tlsManagerSettings
    { managerModifyRequest = \req -> do
        req' <- managerModifyRequest tlsManagerSettings req
        return $
          req'
            { requestHeaders =
                (hUserAgent, "update-linux-torrents")
                  : requestHeaders req'
            }
    }

mostRecentTorrents :: IO (Maybe [URL])
mostRecentTorrents = do
  manager <- Just <$> newManager addUserAgent
  scrapeURLWithConfig (def {manager}) (distroWatch </> "dwres.php?resource=bittorrent") torrentURLs
  where
    torrentURLs :: Scraper String [URL]
    torrentURLs = chroots ("td" @: [hasClass "torrent"]) torrentURL

    torrentURL :: Scraper String URL
    torrentURL = do
      url <- attr "href" "a"
      return url

downloadRecentTorrents :: IO ()
downloadRecentTorrents = do
  torrentURLs <- mostRecentTorrents
  let recent = take 25 $ filter (\x -> ".torrent" `isSuffixOf` x) (fromJust torrentURLs)
  mapM_
    ( \x -> do
        let url = distroWatch </> x
        manager <- newManager addUserAgent

        request <- parseRequest url
        response <- httpLbs request manager

        let filePath = downloads </> (takeFileName x)

        writeFile filePath $ unpack (responseBody response)
    )
    recent

main :: IO ()
main = do
  cleanAddedTorrents
  downloadRecentTorrents
