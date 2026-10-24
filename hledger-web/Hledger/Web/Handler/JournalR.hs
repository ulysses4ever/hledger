-- | /journal handlers.

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Web.Handler.JournalR where

import Data.List (nub)
import Data.Text qualified as T
import Hledger.Utils.I18n (tr, trf)
import Hledger
import Hledger.Cli.CliOptions
import Hledger.Web.Import
import Hledger.Web.Paging
import Hledger.Web.WebOptions
import Hledger.Web.Widget.AddForm (addModal)
import Hledger.Web.Widget.Common
            (accountNameAsHtml, accountQuery, mixedAmountAsHtml,
             transactionFragment, replaceInacct, journalDayQuery)
import Hledger.Web.Widget.Entry
import Hledger.Web.Widget.Period (isAllTime, pagePeriod, periodBand, periodQuery)

-- | The formatted journal view, with sidebar.
getJournalR :: Handler Html
getJournalR = do
  checkServerSideUiEnabled
  VD{perms, j, q, opts, qparam, qopts, today, trs} <- getViewData
  require ViewPermission
  pagereq <- pageRequest
  -- The journal shows each entry single-sided unless "Show both sides" was
  -- ticked, which hledger.js remembers in this cookie.
  bothSides <- (== Just "1") . lookup "journalbothsides" . reqCookies <$> getRequest
  -- The period shown: the search's dates, or the period parameter, or this
  -- month (see pagePeriod).
  mperiod <- lookupGetParam "period"
  shown <- case pagePeriod today (alltime_ opts) qparam q mperiod of
    Right p -> return p
    Left err -> PeriodAll <$ setMessage (toHtml $ "Could not parse the period: " <> T.pack err)
  let title = case inAccount qopts of
        Nothing         -> tr trs "General Journal"
        Just (a, True)  -> trf trs "Transactions in {account}" [("account", a)]
        Just (a, False) -> trf trs "Transactions in {account} (excluding subaccounts)" [("account", a)]
      title' = if q /= Any then trf trs "{title}, filtered" [("title", title)] else title
      -- An account's register, opened on the page holding this transaction.
      acctlink a t = (RegisterR, [("q", replaceInacct qparam $ accountQuery a), ("txn", T.pack $ show $ tindex t)])
      q' = filterQuery (not . queryIsDepth) q
      -- the search, and the period's dates if they are not the search's own
      qp | filterQuery queryIsDateOrDate2 q' /= Any = q'
         | otherwise = simplifyQuery $ And [q', periodQuery shown]
      rspec = (reportspec_ $ cliopts_ opts){_rsQuery = qp}
      -- The matching transactions, newest first; this page shows one page of them.
      alltxns = reverse $
        styleAmounts (journalCommodityStylesWith HardRounding j) $
        entriesReport rspec j
      (page, items) = pageOf pagereq tindex alltxns
      -- The years the search matches in, ignoring its dates and the period's.
      years = map tdate $
        entriesReport rspec{_rsQuery = maybe q' (filterQuery (not . queryIsDepth)) $ datelessQuery today qparam} j
      -- How much the period's entries spent and earned: their postings to
      -- expense accounts, and to revenue accounts, by account type.
      atype = journalAccountType j
      typeTotal ty = sumPostings [p | t <- alltxns, p <- tpostings t, maybe False (`isAccountSubtypeOf` ty) $ atype (paccount p)]
      spent = typeTotal Expense
      earned = maNegate $ typeTotal Revenue
      oneLine = wbToText . showMixedAmountB oneLineNoCostFmt{displayZeroCommodity = True}
      -- the period band, with the totals under it
      -- TRANSLATORS: under the period at the top of the journal, as in
      -- "spent $120.00 · earned $3,200.00": the total of its expenses, and
      -- of its income.
      totals = [hamlet|
        <div .period-totals>
          <span .totals-label>#{tr trs "spent"} #
          <span .amount .spent>#{oneLine spent}
          \ · #
          <span .totals-label>#{tr trs "earned"} #
          <span .amount .earned>#{oneLine earned}
        |]
      noParams = []
      -- While the default is a month, the all-years link asks for all dates.
      yearParams = [("period", "all") | not (alltime_ opts)]
      transactionFrag = transactionFragment j
      -- Each entry, with the summary a phone shows of it, and the
      -- accounts that summary names on its second line: where a payment
      -- came from, where income went, or both, for a transfer.
      entries = [(t, summarizeEntry atype t) | t <- items]
      lineFrom es = if esKind es == Earning then [] else take 1 (esFrom es)
      lineTo es = if esKind es == Spending then [] else take 1 (esTo es)
      bothEnds es = not (null (lineFrom es) || null (lineTo es))
      -- Is this a posting the money came from, its amounts all negative ?
      -- Single-sided, the journal shows those accounts on a line of their
      -- own, without amounts.
      isSource amt = isNegativeMixedAmount amt == Just True
      -- The accounts an entry's money came from, each with the separator
      -- that follows it.
      sourceAccounts :: Transaction -> [(AccountName, Text)]
      sourceAccounts t =
        let accts = nub [paccount p | p <- tpostings t, isSource (pamount p)]
        in zip accts (drop 1 (", " <$ accts) ++ [""])

  defaultLayout $ do
    -- TRANSLATORS: the browser tab title of this page.
    setTitleI (HMsg "journal - hledger-web")
    $(widgetFile "journal")
