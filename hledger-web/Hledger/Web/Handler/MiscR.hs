{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Web.Handler.MiscR
  ( getVersionR
  , getAccountnamesR
  , getTransactionsR
  , getPricesR
  , getCommoditiesR
  , getAccountsR
  , getAccounttransactionsR
  , getDownloadR
  , getFaviconR
  , getCustomCssR
  , getManageR
  , getRobotsR
  , getRootR
  , getOpenApiR
  ) where

import Control.Exception (IOException, try)
import Data.Bits (xor)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Word (Word64)
import Numeric (showHex)
import System.IO (hPutStrLn, stderr)
import Data.Yaml qualified as Yaml
import Data.ByteString qualified as BS
import Data.FileEmbed (embedFile)

import Hledger
import Hledger.Web.Import
import Hledger.Web.WebOptions (WebOpts(..), packageversion)
import Hledger.Web.Widget.Common (journalFile404)

-- | Serve the favicon and robots.txt from copies of config/favicon.ico and
-- config/robots.txt embedded in this executable at build time, so that they
-- are served whatever directory hledger-web is run from.
getFaviconR :: Handler TypedContent
getFaviconR = return $ TypedContent "image/x-icon" $ toContent $(embedFile "config/favicon.ico")

-- | Serve the stylesheet given with --css. It is read from disk on every
-- request, so that edits to it show on the next page load, and is sent, like
-- the static files, with an ETag and Cache-Control: no-cache, so that it
-- costs a 304 response when unchanged. Not found when there is no --css, or
-- the file can't be read (which is reported on stderr).
getCustomCssR :: Handler TypedContent
getCustomCssR = do
  App{appOpts} <- getYesod
  f <- maybe notFound return $ css_ appOpts
  ecss <- liftIO (try $ BS.readFile f :: IO (Either IOException BS.ByteString))
  case ecss of
    Left e -> do
      liftIO $ hPutStrLn stderr $ "hledger-web: could not read the --css file: " ++ show e
      notFound
    Right css -> do
      addHeader "Cache-Control" "no-cache"
      setEtag $ T.pack $ showHex (fnv1a css) ""
      return $ TypedContent "text/css; charset=utf-8" $ toContent css
  where
    -- a cheap hash of the bytes (64-bit FNV-1a), to tell versions apart
    fnv1a :: BS.ByteString -> Word64
    fnv1a = BS.foldl' (\h b -> (h `xor` fromIntegral b) * 1099511628211) 14695981039346656037

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain $ toContent $(embedFile "config/robots.txt")

getRootR :: Handler Html
getRootR = do
  checkServerSideUiEnabled
  redirect JournalR

getManageR :: Handler Html
getManageR = do
  checkServerSideUiEnabled
  VD{j} <- getViewData
  require EditPermission
  defaultLayout $ do
    setTitleI (HMsg "Edit journal")
    $(widgetFile "manage")

getDownloadR :: FilePath -> Handler TypedContent
getDownloadR f = do
  checkServerSideUiEnabled
  VD{j} <- getViewData
  require EditPermission
  (f', txt) <- journalFile404 f j
  addHeader "Content-Disposition" ("attachment; filename=\"" <> T.pack f' <> "\"")
  sendResponse ("text/plain" :: ByteString, toContent txt)

-- hledger-web equivalents of the old hledger-api's handlers

getVersionR :: Handler TypedContent
getVersionR = do
  require ViewPermission
  selectRep $ provideJson $ packageversion

getAccountnamesR :: Handler TypedContent
getAccountnamesR = do
  VD{j} <- getViewData
  require ViewPermission
  selectRep $ provideJson $ journalAccountNames j

getTransactionsR :: Handler TypedContent
getTransactionsR = do
  VD{j} <- getViewData
  require ViewPermission
  selectRep $ provideJson $ jtxns j

getPricesR :: Handler TypedContent
getPricesR = do
  VD{j} <- getViewData
  require ViewPermission
  selectRep $
    provideJson $ map priceDirectiveToMarketPrice $ jpricedirectives j

getCommoditiesR :: Handler TypedContent
getCommoditiesR = do
  VD{j} <- getViewData
  require ViewPermission
  selectRep $ do
    provideJson $ (M.keys . jinferredcommoditystyles) j

getAccountsR :: Handler TypedContent
getAccountsR = do
  VD{j} <- getViewData
  require ViewPermission
  selectRep $ do
    provideJson $
      styleAmounts (journalCommodityStylesWith HardRounding j) $
      flattenAccounts $ mapAccounts (accountSetDeclarationInfo j) $ ledgerRootAccount $ ledgerFromJournal Any j

getAccounttransactionsR :: Text -> Handler TypedContent
getAccounttransactionsR a = do
  VD{j} <- getViewData
  require ViewPermission
  let
    rspec = defreportspec
    thisacctq = Acct $ accountNameToAccountRegex a -- includes subs
  selectRep $ do
    provideJson $
      styleAmounts (journalCommodityStylesWith HardRounding j) $
      accountTransactionsReport rspec{_rsQuery=Any} j thisacctq

openApiYaml :: BS.ByteString
openApiYaml = $(embedFileRelative "config/openapi.yaml")

getOpenApiR :: Handler Value
getOpenApiR =
  case Yaml.decodeEither' openApiYaml of
    Left _ -> notFound
    Right openapi -> do
      addHeader "Content-Type" "application/json"
      return openapi
