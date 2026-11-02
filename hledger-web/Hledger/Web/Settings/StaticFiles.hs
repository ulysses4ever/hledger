{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
Serving of the web app's static files, and type-safe routes for them.
-}
module Hledger.Web.Settings.StaticFiles where

import Data.ByteString.Char8 qualified as B8
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Language.Haskell.TH (runIO, stringE)
import Data.FileEmbed (embedDir)
import Data.Maybe (fromMaybe)
import Network.Wai (Middleware, mapResponseHeaders, requestHeaders)
import Network.Wai.Application.Static (defaultFileServerSettings, embeddedSettings, staticApp)
import System.IO (stdout, hFlush)
import WaiAppStatic.Types (MaxAge(..), StaticSettings(..))
import Yesod.Core (WaiSubsite(..))

import Hledger.Web.Settings (staticDir, development, staticFileRoute)

-- | A stamp of this build, put in the page's links to its own script and
-- stylesheet, so that a browser never keeps using a copy cached before a
-- build (the files are also served with no-cache, which not every browser
-- honours).
staticVersion :: Text
staticVersion = $(stringE . show . (floor :: POSIXTime -> Integer) =<< runIO getPOSIXTime)

-- | Create the static file serving site (a WAI app used as a Yesod subsite).
-- In development builds it serves the files in the static directory,
-- re-reading them from disk on each request;
-- otherwise it serves the static directory's files as they were
-- at compile time, embedded in the executable.
staticSite :: IO WaiSubsite
staticSite =
  if development
   then do
     putStrLn ("Running in dev mode, will read static files from " ++ staticDir ++ "/") >> hFlush stdout
     return $ serve $ defaultFileServerSettings staticDir
   else
     -- putStrLn "Using built-in web files" >> hFlush stdout
     return $ serve $ embeddedSettings $(embedDir staticDir)
  where
    -- Serve files with hash-based ETag headers, and Cache-Control: no-cache,
    -- so that browsers and proxies may keep them but must check them on each
    -- use, which for an unchanged file costs a 304 response. Their urls do not
    -- change from one version of hledger-web to the next, so a cache that did
    -- not check could go on using an old version's files after an upgrade.
    serve settings = WaiSubsite $ quoteETags $ staticApp settings{ssUseHash = True, ssMaxAge = revalidate}
#if MIN_VERSION_wai_app_static(3,1,9)
    revalidate = NoCache
#else
    revalidate = MaxAgeSeconds 0
#endif

-- | wai-app-static sends its file hashes as bare ETag values, which are not
-- valid entity tags (they should be quoted), and which a proxy may drop, or
-- no longer match once it has made them weak (W/"..."), as some do when they
-- compress a response. So quote them on the way out, and take such a tag
-- back to the bare hash on the way in.
quoteETags :: Middleware
quoteETags app req respond =
  app req{requestHeaders = map unquote (requestHeaders req)} (respond . mapResponseHeaders (map quote))
  where
    quote (h, v) | h == "ETag" = (h, B8.concat ["\"", v, "\""])
    quote hv = hv
    unquote (h, v) | h == "If-None-Match" = (h, B8.filter (/= '"') $ fromMaybe v $ B8.stripPrefix "W/" v)
    unquote hv = hv

-- Type-safe routes for the static files used by the app,
-- verified to exist at compile time.
css_bootstrap_min_css           = $(staticFileRoute "css/bootstrap.min.css")
js_jquery_min_js                = $(staticFileRoute "js/jquery.min.js")
js_jquery_flot_min_js           = $(staticFileRoute "js/jquery.flot.min.js")
js_jquery_flot_selection_min_js = $(staticFileRoute "js/jquery.flot.selection.min.js")
js_jquery_flot_time_min_js      = $(staticFileRoute "js/jquery.flot.time.min.js")
js_jquery_flot_tooltip_min_js   = $(staticFileRoute "js/jquery.flot.tooltip.min.js")
hledger_css                     = $(staticFileRoute "hledger.css")
hledger_js                      = $(staticFileRoute "hledger.js")
