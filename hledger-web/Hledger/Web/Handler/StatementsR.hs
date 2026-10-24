-- | The financial statement pages: /balancesheet, /balancesheetequity,
-- /incomestatement, and /cashflow, showing the reports of the commands
-- of the same names.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Hledger.Web.Handler.StatementsR (
  getBalancesheetR,
  getBalancesheetequityR,
  getIncomestatementR,
  getCashflowR,
) where

import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Yesod qualified
import Data.List (elemIndex)
import Data.Text qualified as T

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Commands.Balancesheet (balancesheetSpec)
import Hledger.Cli.Commands.Balancesheetequity (balancesheetequitySpec)
import Hledger.Cli.Commands.Cashflow (cashflowSpec)
import Hledger.Cli.Commands.Incomestatement (incomestatementSpec)
import Hledger.Cli.CompoundBalanceCommand
import Hledger.Utils.I18n (Translations, i18n, tr, trf)
import Hledger.Write.Spreadsheet qualified as Spr

import Hledger.Web.Import
import Hledger.Web.ReportPage
import Hledger.Web.WebOptions
import Hledger.Web.Widget.Common (accountNameHtml, helplink, intervalLinks, linksRow, removeInacct, reportChoices, reportLinks)

getBalancesheetR, getBalancesheetequityR, getIncomestatementR, getCashflowR :: Handler Html
-- TRANSLATORS: the browser tab titles of the statement pages.
-- Each opens on one of its sections (see statementPage): the balance
-- sheets on assets, the income statement on expenses, which is what a
-- money app's statistics open on.
getBalancesheetR       = statementPage BalancesheetR       (i18n "balance sheet - hledger-web")             balancesheetSpec       0
getBalancesheetequityR = statementPage BalancesheetequityR (i18n "balance sheet with equity - hledger-web") balancesheetequitySpec 0
getIncomestatementR    = statementPage IncomestatementR    (i18n "income statement - hledger-web")          incomestatementSpec    1
getCashflowR           = statementPage CashflowR           (i18n "cashflow statement - hledger-web")        cashflowSpec           0

-- | Which of a statement's sections a page shows.
data ShownSections = OneSection Int | AllSections
  deriving (Eq)

-- | A statement page: the command's report for the page's search and
-- period, with sidebar. The report's own accumulation mode applies
-- (ending balances for the balance sheets, changes for the others)
-- unless an accum parameter overrides it, which the heading then says,
-- as the command's does.
--
-- A statement with several sections shows one at a time, the given one
-- unless the section parameter names another (by its title in the
-- command, in lower case with hyphens: assets, liabilities, equity,
-- revenues, expenses), with its total as the table's; or all of them, as
-- the command does, with the net total (section=all). A row of links, or
-- on a phone a drop-down list, picks one. Shown alone, a section needs no
-- title row: the list names it.
statementPage :: AppRoute -> Text -> CompoundBalanceCommandSpec -> Int -> Handler Html
statementPage here tabtitle spec defaultSection = do
  checkServerSideUiEnabled
  VD{j, q, qopts, qparam, opts, today, trs} <- getViewData
  require ViewPermission
  params <- reqGetParams <$> getRequest
  hideEmpty <- hideEmptyAccounts
  urlrender <- getUrlRenderParams
  let dateless = filterQuery (not . queryIsDateOrDate2)
      withFilter t = if dateless q /= Any then trf trs "{title}, filtered" [("title", t)] else t
      rspecOrig = reportspec_ $ cliopts_ opts
      roptsOrig = _rsReportOpts rspecOrig
      menu = reportLinkItems reportMenu

  defaultLayout $ do
    setTitleI (HMsg tabtitle)
    case reportParams today rspecOrig qparam q qopts hideEmpty params of
      Left err -> Yesod.toWidget $ do
        H.h2 $ H.toHtml $ withFilter $ effectiveTitle roptsOrig $ tr trs $ cbctitle spec NoInterval
        paramError trs err
      Right ReportParams{rpRopts, rpRspec, rpSpan, rpInterval, rpPeriod, rpAccum} -> do
        let accum = fromMaybe (cbcaccum spec) rpAccum
            override = mfilter (/= cbcaccum spec) rpAccum
            ropts = rpRopts{balanceaccum_ = accum}
            rspec = rpRspec{_rsReportOpts = ropts}
            cbr0 =
              styleAmounts (journalCommodityStylesWith HardRounding j) $
                compoundBalanceReport rspec j (cbcqueries spec)
            cbr =
              applySubreportTitles ropts $
                cbr0{cbrTitle = effectiveTitle ropts $ compoundBalanceReportTitle spec rpRopts override cbr0}
            colspans = cbrDates cbr
            -- What links staying on this page keep: the period as given, and
            -- the mode when it is not the report's own.
            periodParams = [("period", p) | Just p <- [rpPeriod]]
            accumParams = [("accum", if accum == Historical then "historical" else "change") | isJust override]
            -- Links to the other reports keep the search, minus any account
            -- term, which the reports ignore, and the period as given.
            menuParams = periodParams ++ [("q", qt) | let qt = T.unwords $ removeInacct qparam, not (T.null qt)]
            headinglink = urlrender here . headingParams qparam params
            CompoundBalanceReportParts{cbrpLeadingHeaders, cbrpDataHeaders, cbrpSections, cbrpNetRows} =
              compoundBalanceReportAsSpreadsheetParts oneLineNoCostFmt "account" ropts (cbcqueries spec) cbr
            -- The heading row with the classes the stylesheet aligns the
            -- columns by, and this page's column headings and links.
            withClass c cell = cell{Spr.cellClass = Spr.Class c}
            header =
              map (withClass "account") cbrpLeadingHeaders ++
              relinkDateHeaders trs (columnHeading ropts colspans) headinglink colspans (map (withClass "amount") cbrpDataHeaders)
            sections = [(mfilter (not . T.null) (Just title), body, subtotals) | (title, body, subtotals) <- cbrpSections]
            -- The sections, each with the key the section parameter names it
            -- by, from the command's own title, and the name the page gives
            -- it (which --subreport-titles may have changed, or blanked).
            sectionKeys = map (T.intercalate "-" . T.words . T.toLower . cbcsubreporttitle) (cbcqueries spec)
            sectionNames = zipWith (\sub (title, _, _) -> if T.null title then tr trs (cbcsubreporttitle sub) else title) (cbcqueries spec) cbrpSections
            several = length cbrpSections > 1
            shown
              | not several = OneSection 0
              | otherwise = case lookup "section" params of
                  Just "all" -> AllSections
                  Just k | Just i <- elemIndex k sectionKeys -> OneSection i
                  _ -> OneSection defaultSection
            -- The parameter for showing the given sections: none for the
            -- page's own section.
            sectionParam = \case
              OneSection i | i == defaultSection -> []
                           | otherwise -> [("section", k) | k <- take 1 (drop i sectionKeys)]
              AllSections -> [("section", "all")]
            -- Links to the sections keep the search, the period and the mode.
            sectionLinks =
              -- TRANSLATORS: the label before a statement's section links, and the links' tooltips.
              linksRow (tr trs "Section:") $
                [ (name, tr trs "Show only this section", (here, sectionParam (OneSection i) ++ kept), shown == OneSection i)
                | (i, name) <- zip [0..] sectionNames ] ++
                [ (tr trs "All", tr trs "Show every section, and the net total", (here, sectionParam AllSections ++ kept), shown == AllSections) ]
              where kept = periodParams ++ accumParams ++ [("q", qparam) | not (T.null qparam)]
            (shownSections, footer) = case shown of
              AllSections -> (sections, cbrpNetRows)
              OneSection i -> case drop i cbrpSections of
                (_, body, subtotals) : _ -> ([(Nothing, body, [])], subtotals)
                [] -> (sections, cbrpNetRows)
            noRows = all (\(_, r, _) -> null $ prRows r) . cbrSubreports
            empty = noRows cbr
            -- An empty report with zero balances hidden may have them all: look again showing them.
            zerosHidden = hideEmpty && not (noRows $ compoundBalanceReport rspec{_rsReportOpts = ropts{empty_ = True}} j (cbcqueries spec))
        Yesod.toWidget $ H.h2 $ H.toHtml $ withFilter $ cbrTitle cbr
        Yesod.toWidget $ reportChoices $
          [reportLinks trs here menuParams menu] ++
          [sectionLinks | several] ++
          [intervalLinks trs here (accumParams ++ sectionParam shown) qparam rpSpan rpInterval]
        if empty
          then Yesod.toWidget $ emptyNotice trs zerosHidden (q == Any && rpSpan == nulldatespan)
          -- The sections name the accounts' top levels (Assets, Liabilities,
          -- ...), so the names leave them out, as the journal's do.
          else Yesod.toWidget $ reportTable (accountNameHtml True) [header] shownSections footer

-- | What an empty statement shows instead of a table: that its accounts
-- all have zero balances, which are hidden; or, for the whole journal,
-- that it has no accounts of the types the report shows, with a pointer
-- to how types are found; or, for a search or a period, that nothing
-- matched.
emptyNotice :: Translations -> Bool -> Bool -> HtmlUrl AppRoute
emptyNotice trs zerosHidden whole render =
  H.div ! A.class_ "alert alert-info" $
    if zerosHidden then H.toHtml $ tr trs "All the accounts this report shows have zero balances, which are hidden. Press e to show them."
    else if whole
      then do
        H.toHtml $ tr trs "No accounts of the types this report shows were found. Declare account types, or use hledger's standard top-level account names. "
        helplink "account-types" (tr trs "How hledger finds account types") render
      else H.toHtml $ tr trs "Nothing matches this search in this period."
