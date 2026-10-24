-- | /register handlers.

{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Hledger.Web.Handler.RegisterR where

import Data.Aeson.Text (encodeToLazyText)
import Data.Function (on)
import Data.List (nub, partition)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Safe (tailSafe)
import Text.Hamlet (hamletFile)

import Hledger.Utils.I18n (tr, trc, trf)
import Hledger
import Hledger.Cli.CliOptions
import Hledger.Web.Import
import Hledger.Web.Paging
import Hledger.Web.WebOptions
import Hledger.Web.Widget.AddForm (addModal)
import Hledger.Web.Widget.Common
             (accountNameAsHtml, accountQuery, accountOnlyQuery, mixedAmountAsHtml,
              transactionFragment, removeDates, removeInacct, replaceInacct, journalDayQuery)
import Hledger.Web.Widget.Entry
            (EntrySummary(..), dayWithWeekday, entryKindClass, entryMainOtherAccount, payeeAndNote, summarizeEntry)
import Hledger.Web.Widget.Period (isAllTime, pagePeriod, periodBand, periodQuery)

-- | The main journal/account register view, with accounts sidebar.
getRegisterR :: Handler Html
getRegisterR = do
  checkServerSideUiEnabled
  VD{perms, j, q, opts, qparam, qopts, today, trs} <- getViewData
  require ViewPermission
  -- With accum=historical the running balance is the account's balance,
  -- starting from before the query's start date, rather than a total of
  -- the transactions shown; a report's ending balance links here that way,
  -- so that the balance ends on the figure clicked.
  historical <- (== Just "historical") <$> lookupGetParam "accum"
  pagereq <- pageRequest
  -- The period shown: the search's dates, or the period parameter, or this
  -- month (see pagePeriod).
  mperiod <- lookupGetParam "period"
  shown <- case pagePeriod today (alltime_ opts) qparam q mperiod of
    Right p -> return p
    Left err -> PeriodAll <$ setMessage (toHtml $ "Could not parse the period: " <> T.pack err)

  let title = case inAccount qopts of
        Nothing         -> tr trs "all accounts"
        Just (a, True)  -> a
        Just (a, False) -> trf trs "{account} (excluding subaccounts)" [("account", a)]
      header = if q /= Any then trf trs "{title}, filtered" [("title", title)] else title
      -- An account's own page names it in full in its heading, its parts
      -- set apart as in the rows, but keeping the top level.
      headingAccount = case inAccount qopts of
        Just (a, True) | q == Any -> Just a
        _ -> Nothing

  let rspec0 = reportspec_ (cliopts_ opts)
      ropts = (_rsReportOpts rspec0){balanceaccum_ = if historical then Historical else PerPeriod}
      rspec = rspec0{_rsReportOpts = ropts}
      -- links staying on this register keep its mode
      accumParams = [("accum", "historical") | historical]
      qParams t = [("q", t) | not (T.null t)]
      acctQuery = fromMaybe Any (inAccountQuery qopts)
      -- An account's register, keeping this register's mode, opened on the
      -- page holding this transaction.
      acctlink acc t = (RegisterR, ("q", replaceInacct qparam $ accountQuery acc) : ("txn", T.pack $ show $ tindex t) : accumParams)
      -- The accounts a row names beside the one it is for: as many as fit
      -- in about forty characters as they are shown, each whole, with the
      -- separator that follows each.
      -- In an account's register a type: term selects the postings
      -- totaled, not the accounts named beside them: a liability's
      -- register names the accounts it was posted against, whatever their
      -- types. A register of all accounts of a type names those accounts.
      otherTransAccounts =
          fitAccountNames 40 . nub . map paccount . preferReal . otherTransactionAccounts j displayq acctQuery
      displayq = if isJust (inAccount qopts) then filterQuery (not . queryIsType) q else q
      styles = journalCommodityStylesWith HardRounding j
      -- the search, and the period's dates if they are not the search's own
      qp | filterQuery queryIsDateOrDate2 q /= Any = q
         | otherwise = simplifyQuery $ And [q, periodQuery shown]
      -- The matching transactions, newest first, and the balance brought
      -- forward from before them; this page shows one page of them.
      (startbal, allitems) =
        bimap (styleAmounts styles) (styleAmounts styles) $
        accountTransactionsReportWithStart rspec{_rsQuery=qp} j acctQuery
      (page, items) = pageOf pagereq (tindex . triOrigTransaction) allitems
      -- The brought-forward row is older than every transaction, so it
      -- ends the last page.
      lastpage = pgNumber page >= pgCount page
      -- The years the search matches in, ignoring any date term in it. A
      -- year's count is by register date, while its page matches date:YYYY
      -- against each posting's date, so a transaction with postings dated in
      -- two years is counted once and shown in both.
      years = map triDate $
        accountTransactionsReport rspec{_rsQuery = fromMaybe q $ datelessQuery today qparam} j acctQuery
      balancelabel
        | historical               = trc trs "column heading" "Historical Total"
        | isJust (inAccount qopts) = trc trs "column heading" "Period Total"
        | otherwise                = trc trs "column heading" "Total"
      -- The balance column's heading switches the mode.
      accumToggle = (RegisterR, qParams qparam ++ [("accum", "historical") | not historical])
      accumToggleTitle
        | historical = tr trs "Show the running balance from the start of this period"
        | otherwise  = tr trs "Show the running balance including everything before this period"
      -- In historical mode with a start date, the balance brought forward
      -- from before it is the oldest row, linking to the transactions
      -- before the period: those before the start date by the kind of
      -- date (primary or secondary) the report took it from, as the
      -- report chooses the balance's cutoff.
      mstart = asum [ (,) secondary <$> queryStartDate secondary qp
                    | secondary <- [date2_ ropts, not $ date2_ ropts] ]
      broughtForwardLink (secondary, start) =
        (RegisterR, [("q", T.unwords $
          maybe [] (\(acc, incl) -> [if incl then accountQuery acc else accountOnlyQuery acc]) (inAccount qopts) ++
          [(if secondary then "date2:.." else "date:..") <> showDate start] ++
          removeDates (T.unwords $ removeInacct qparam))])
      transactionFrag = transactionFragment j
      -- What each row's entry did, and the account it is mainly about
      -- other than this register's.
      summary = summarizeEntry (journalAccountType j)
      mainAccount = entryMainOtherAccount acctQuery . summary
      -- The account page's summary of the period, of all its matching
      -- transactions, not only this page's: the balance before it, at its
      -- end, and the money paid in and out during it.
      isAccountPage = isJust (inAccount qopts)
      prevbal
        | historical = startbal
        | otherwise  = styleAmounts styles $ fst $
            accountTransactionsReportWithStart
              rspec{_rsQuery=qp, _rsReportOpts=ropts{balanceaccum_=Historical}} j acctQuery
      -- A row's balance, including everything before the period, whatever
      -- the mode: on a phone, each row shows it.
      runningBalance bal = if historical then bal else prevbal `maPlus` bal
      endbal = case allitems of
        (_, _, _, _, _, bal) : _ -> runningBalance bal
        []                       -> prevbal
      paidin = maSum [filterMixedAmount ((> 0) . aquantity) amt | (_, _, _, _, amt, _) <- allitems]
      paidout = maSum [filterMixedAmount ((< 0) . aquantity) amt | (_, _, _, _, amt, _) <- allitems]
      oneLine = wbToText . showMixedAmountB oneLineNoCostFmt{displayZeroCommodity = True}
      isNegative amt = isNegativeMixedAmount amt == Just True
      isPositive amt = isNegativeMixedAmount amt == Just False && not (mixedAmountLooksZero amt)
      -- This page's rows by day, newest first, with each day's total: on a
      -- phone they are grouped under a heading per day.
      days = [ (rowDay r, maSum [amt | (_, _, _, _, amt, _) <- r : rs], r : rs)
             | r :| rs <- NE.groupBy ((==) `on` rowDay) items ]
      rowDay (_, tacct, _, _, _, _) = tdate tacct
      -- The year links keep this register's mode, and while the default
      -- is a month, the all-years link asks for all dates.
      yearParams = [("period", "all") | not (alltime_ opts)] ++ accumParams
      noBelow = const mempty
  defaultLayout $ do
    -- TRANSLATORS: the browser tab title of this page.
    setTitleI (HMsg "register - hledger-web")
    $(widgetFile "register")

-- cf. Hledger.Reports.AccountTransactionsReport.accountTransactionsReportItems
otherTransactionAccounts :: Journal -> Query -> Query -> Transaction -> [Posting]
otherTransactionAccounts j reportq thisacctq torig
    -- no current account ? summarise all matched postings
    | thisacctq == None  = reportps
    -- only postings to current account ? summarise those
    | null otheraccts    = thisacctps
    -- summarise matched postings to other account(s)
    | otherwise          = otheracctps
    where
      -- given the account types, so that a type: term matches postings here as in the report
      reportps = tpostings $ filterTransactionPostingsExtra (journalAccountType j) reportq torig
      (thisacctps, otheracctps) = partition (matchesPosting thisacctq) reportps
      otheraccts = nub $ map paccount otheracctps

-- cf. Hledger.Reports.AccountTransactionsReport.summarisePostingAccounts
preferReal :: [Posting] -> [Posting]
preferReal ps
    | null realps = ps
    | otherwise   = realps
    where realps = filter isReal ps

-- | As many of these account names as fit in the given number of
-- characters, taking each as wide as the journal and register show it
-- (without its top level, with " › " between the rest), but at least one;
-- each paired with the separator that follows it: a comma, nothing after
-- the last, or a comma and an ellipsis if some were left out.
fitAccountNames :: Int -> [AccountName] -> [(AccountName, Text)]
fitAccountNames width names = zip shown (tailSafe (", " <$ shown) ++ [if length shown < length names then ", …" else ""])
  where
    shown = take (max 1 . length . takeWhile (<= width) . scanl1 (\total w -> total + 2 + w) $ map shownWidth names) names
    shownWidth a = case accountNameComponents a of
      _ : rest@(_ : _) -> sum (map T.length rest) + 3 * (length rest - 1)
      _ -> T.length a

-- | The register balance chart: its markup, carrying the per-commodity
-- series as JSON in a data attribute. hledger.js draws it with flot on page
-- load; see registerChartInit there.
registerChartHtml ::
  Text -> [(Text, Text)] -> (Transaction -> String) -> String ->
  [(CommoditySymbol, [AccountTransactionsReportItem])] -> HtmlUrl AppRoute
registerChartHtml q accumParams transactionFrag title percommoditytxnreports = $(hamletFile "templates/chart.hamlet")
 where
   charttitle = if null title then "" else title ++ ":"
   nodatelink = (RegisterR, [("q", t) | let t = T.unwords $ removeDates q, not (T.null t)] ++ accumParams)
   -- One entry per commodity: its symbol, and per transaction the point flot
   -- plots followed by the texts the tooltip and click handler show.
   seriesjson = encodeToLazyText $ map commoditySeries percommoditytxnreports
   commoditySeries (c, items) = object
     [ "label"  .= c
     , "points" .= [ [ toJSON . dayToUtcNoonTimestamp $ triDate i
                     , toJSON . quantityAsDouble $ triCommodityBalance c i
                     , toJSON . showZeroCommodity $ triCommodityAmount c i
                     , toJSON . showZeroCommodity $ triCommodityBalance c i
                     , toJSON . T.stripEnd . showTransaction $ triOrigTransaction i
                     , toJSON . transactionFrag $ triOrigTransaction i
                     ]
                   | i <- reverse items ]
     ]
   -- The first amount's quantity, or 0. (Decimal's own ToJSON instance is an
   -- object; the chart wants a plain number.)
   quantityAsDouble :: MixedAmount -> Double
   quantityAsDouble = maybe 0 (realToFrac . aquantity) . listToMaybe . amounts . mixedAmountStripCosts
   showZeroCommodity = wbUnpack . showMixedAmountB oneLineNoCostFmt{displayCost=False,displayZeroCommodity=True}

-- | Makes a unix timestamp (milliseconds since epoch) corresponding to noon on the given date in UTC.
dayToUtcNoonTimestamp :: Day -> Integer
dayToUtcNoonTimestamp d =
  read (formatTime defaultTimeLocale "%s" t) * 1000 -- XXX read
  where
    t = UTCTime d (secondsToDiffTime $ 12 * 60 * 60)
