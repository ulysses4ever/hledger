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
  let title = case inAccount qopts of
        Nothing         -> tr trs "General Journal"
        Just (a, True)  -> trf trs "Transactions in {account}" [("account", a)]
        Just (a, False) -> trf trs "Transactions in {account} (excluding subaccounts)" [("account", a)]
      title' = if q /= Any then trf trs "{title}, filtered" [("title", title)] else title
      -- An account's register, opened on the page holding this transaction.
      acctlink a t = (RegisterR, [("q", replaceInacct qparam $ accountQuery a), ("txn", T.pack $ show $ tindex t)])
      q' = filterQuery (not . queryIsDepth) q
      rspec = (reportspec_ $ cliopts_ opts){_rsQuery = q'}
      -- The matching transactions, newest first; this page shows one page of them.
      alltxns = reverse $
        styleAmounts (journalCommodityStylesWith HardRounding j) $
        entriesReport rspec j
      (page, items) = pageOf pagereq tindex alltxns
      -- The years the search matches in, ignoring any date term in it.
      years = map tdate $
        maybe alltxns (\dq -> entriesReport rspec{_rsQuery = filterQuery (not . queryIsDepth) dq} j) $
        datelessQuery today qparam
      transactionFrag = transactionFragment j
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
