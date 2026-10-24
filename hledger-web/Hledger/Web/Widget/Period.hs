{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
The period a journal or register page shows, and the band at the page's
top which names it, with links to the periods before and after.
-}
module Hledger.Web.Widget.Period
  ( pagePeriod
  , periodQuery
  , isAllTime
  , periodBand
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day, defaultTimeLocale, formatTime, fromGregorian, toGregorian)
import Text.Hamlet (hamletFile)
import Text.Megaparsec.Error (errorBundlePretty)
import Yesod (HtmlUrl)

import Hledger
import Hledger.Query qualified as Query
import Hledger.Utils.I18n (Translations, tr, trTimeLocale)
import Hledger.Web.Widget.Common (removeDates)

-- | The period a journal or register page shows. Date terms in its search
-- decide it, as they always did. Otherwise a period parameter does: "all",
-- or a period expression, such as a month (2026-10) or a year. Otherwise,
-- when the page is being browsed rather than searched (its search names
-- an account at most), it is the month of today, unless the default is
-- all dates (--all-time); a search, like the default, shows all its matches.
pagePeriod :: Day -> Bool -> Text -> Query -> Maybe Text -> Either String Period
pagePeriod today alltime qparam q mparam
  | any isDateTerm terms = Right $ dateSpanAsPeriod $ queryDateSpan (not $ any isPrimaryDateTerm terms) q
  | Just "all" <- mparam = Right PeriodAll
  | Just p <- mparam, not (T.null p) =
      either (Left . errorBundlePretty) (Right . dateSpanAsPeriod . snd) $ parsePeriodExpr today p
  | browsing && not alltime = Right $ MonthPeriod y m
  | otherwise = Right PeriodAll
  where
    terms = filter (not . T.null) $ Query.words'' queryprefixes qparam
    isPrimaryDateTerm = ("date:" `T.isPrefixOf`)
    isDateTerm t = isPrimaryDateTerm t || "date2:" `T.isPrefixOf` t
    browsing = all (\t -> any (`T.isPrefixOf` t) ["inacct:", "inacctonly:"]) terms
    (y, m, _) = toGregorian today

-- | The query that selects a period's dates, to be added to the page's
-- search when the period did not come from it.
periodQuery :: Period -> Query
periodQuery PeriodAll = Any
periodQuery p = Date $ periodAsDateSpan p

-- | Is this all dates ?
isAllTime :: Period -> Bool
isAllTime = (== PeriodAll)

-- | The band at the top of a journal or register page: the period shown,
-- with links to the one before and the one after when it is a day, month,
-- quarter or year, and whatever the page puts under it (the journal, how
-- much was spent and earned). The links keep the search, without its date
-- terms, and the given parameters.
periodBand :: Translations -> r -> Text -> [(Text, Text)] -> Period -> HtmlUrl r -> HtmlUrl r
periodBand trs here qparam kept shown below = $(hamletFile "templates/period-band.hamlet")
  where
    label = case shown of
      MonthPeriod y m -> T.pack $ formatTime (trTimeLocale trs) "%B %Y" (fromGregorian y m 1)
      DayPeriod d     -> T.pack $ formatTime defaultTimeLocale "%F (%a)" d
      PeriodAll       -> tr trs "All dates"
      p               -> showPeriod p
    stepping = case shown of
      DayPeriod _     -> True
      MonthPeriod _ _ -> True
      QuarterPeriod _ _ -> True
      YearPeriod _    -> True
      _               -> False
    rest = removeDates qparam
    link p = (here, [("q", T.unwords rest) | not (null rest)] ++ [("period", showPeriod p)] ++ kept)
    mprev = if stepping then Just (link $ periodPrevious shown) else Nothing
    mnext = if stepping then Just (link $ periodNext shown) else Nothing
