-- | The breakdown page: where the money went in a period, as a ring chart
-- and a list, drilled into step by step, as phone money apps show it. The
-- first page splits the period into income, expenses and transfers; each
-- opens on its categories, a category on its subcategories, and an
-- account with nothing under it on its register for the period. The
-- period is chosen as on the journal and the registers (the band).

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Web.Handler.BreakdownR
  ( getBreakdownR
  ) where

import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Ord (Down(..))
import Data.Text qualified as T
import Text.Printf (printf)

import Hledger
import Hledger.Utils.I18n (tr)
import Hledger.Web.Import
import Hledger.Web.WebOptions (WebOpts(..))
import Hledger.Web.Widget.Common (accountNameAsHtml, accountQuery)
import Hledger.Web.Widget.Period (pagePeriod, periodBand, periodParam)

-- | One part of the breakdown: what it is called, how much, and where it leads.
data Slice = Slice
  { sLabel  :: HtmlUrl AppRoute
  , sAmount :: MixedAmount
  , sQty    :: Quantity
  , sLink   :: (AppRoute, [(Text, Text)])
  , sColour :: Maybe Int   -- ^ a colour of its own (the kinds); else by rank
  }

-- | The kinds of money movement the first page splits a period into.
data Kind = Expenses | Income | Transfer deriving (Eq)

kindKey :: Kind -> Text
kindKey Expenses = "expenses"
kindKey Income = "income"
kindKey Transfer = "transfer"

getBreakdownR :: Handler Html
getBreakdownR = do
  checkServerSideUiEnabled
  VD{j, q, qparam, opts, today, trs} <- getViewData
  require ViewPermission
  mkindparam <- lookupGetParam "kind"
  macct <- mfilter (not . T.null) <$> lookupGetParam "acct"
  mperiod <- lookupGetParam "period"
  shown <- case pagePeriod today (alltime_ opts) qparam q mperiod of
    Right p -> return p
    Left err -> PeriodAll <$ setMessage (toHtml $ "Could not parse the period: " <> T.pack err)
  let
    mkind = case mkindparam of
      Just "expenses" -> Just Expenses
      Just "income" -> Just Income
      Just "transfer" -> Just Transfer
      _ -> Nothing
    kindLabel Expenses = tr trs "Expense"
    kindLabel Income = tr trs "Income"
    kindLabel Transfer = tr trs "Transfer"
    spn = periodAsDateSpan shown
    atype = journalAccountType j
    isType ty a = maybe False (`isAccountSubtypeOf` ty) (atype a)
    isExpense = isType Expense
    isRevenue = isType Revenue
    -- The period's entries, as of today, matching the search (its dates
    -- chose the period).
    dateless = filterQuery (not . queryIsDateOrDate2) q
    txns = [ t | t <- jtxns j, spanContainsDate spn (tdate t), tdate t <= today, matchesTransaction dateless t ]
    styles = journalCommodityStylesWith HardRounding j
    onlyReal = isReal
    -- a posting's amount as one number, for the shares: its cost, in one
    -- commodity when it is one, else the quantities added up
    qty ma = case unifyMixedAmount (mixedAmountCost ma) of
      Just a -> aquantity a
      Nothing -> sum (map aquantity (amounts ma))
    qParams = [("q", qparam) | not (T.null qparam)]
    kept = [("kind", kindKey k) | Just k <- [mkind]] ++ [("acct", a) | Just a <- [macct]]
    -- links on this page keep the period, so that the view stays put
    link ps = (BreakdownR, ps ++ [("period", periodParam shown)] ++ qParams)
    registerLink a = (RegisterR, [("q", accountQuery a), ("period", periodParam shown)])
    textLabel t = const (toHtml t)
    oneLine = wbToText . showMixedAmountB oneLineNoCostFmt{displayZeroCommodity = True}
    -- The expense and revenue postings, and the transfers: the entries that
    -- touch neither, counted by what arrives in each account.
    expensePs = [p | t <- txns, p <- tpostings t, onlyReal p, isExpense (paccount p)]
    revenuePs = [p | t <- txns, p <- tpostings t, onlyReal p, isRevenue (paccount p)]
    transferPs = [ p | t <- txns, not (any (\p -> isExpense (paccount p) || isRevenue (paccount p)) (tpostings t))
                     , p <- tpostings t, onlyReal p, qty (pamount p) > 0 ]
    total ps = styleAmounts styles $ maSum (map pamount ps)
    kindSlice k ps negated =
      let amt = (if negated then maNegate else id) (total ps)
      in Slice (textLabel (kindLabel k)) amt (qty amt) (link [("kind", kindKey k)]) (Just (kindColour k))
    -- the kinds' colours, always the same: income blue, transfers yellow, expenses red
    kindColour Income = 1
    kindColour Transfer = 2
    kindColour Expenses = 0
    -- The accounts under an account (or a kind's top levels), each with the
    -- postings on it and below it, grouped by the next part of the name.
    grouped :: Int -> [Posting] -> [(AccountName, [Posting])]
    grouped n ps = M.toList $ M.fromListWith (flip (++))
      [ (T.intercalate ":" (take (n + 1) (accountNameComponents (paccount p))), [p]) | p <- ps ]
    -- A group leads deeper while something is posted below its account;
    -- else to its register for the period.
    groupSlice negated (a, ps) =
      let amt = (if negated then maNegate else id) (total ps)
          deeper = any ((> length (accountNameComponents a)) . length . accountNameComponents . paccount) ps
          target = if deeper then link ([("kind", kindKey k) | Just k <- [mkind]] ++ [("acct", a)]) else registerLink a
      in Slice (accountNameAsHtml a) amt (qty amt) target Nothing
    under a ps = [p | p <- ps, paccount p == a || (a <> ":") `T.isPrefixOf` paccount p]
    slices0 = case (mkind, macct) of
      (Nothing, _) ->
        [ kindSlice Income revenuePs True
        , kindSlice Transfer transferPs False
        , kindSlice Expenses expensePs False ]
      (Just Transfer, _) -> [ groupSlice False (a, ps) | (a, ps) <- grouped 99 transferPs ]
      (Just k, Nothing) ->
        let ps = if k == Income then revenuePs else expensePs
        in map (groupSlice (k == Income)) (grouped 1 ps)
      (Just k, Just a) ->
        let ps = under a (if k == Income then revenuePs else expensePs)
        in map (groupSlice (k == Income)) (grouped (length (accountNameComponents a)) ps)
    slices = sortOn (Down . sQty) [s | s <- slices0, sQty s /= 0]
    grand = sum (map sQty slices)
    shareOf s = if grand == 0 then 0 else 100 * sQty s / grand :: Quantity
    -- The ring: each slice a circle's stroke, dashed to its share, turned
    -- to start where the one before ended, from the top, clockwise. The
    -- circumference is 100, so a share is a dash length. Labels sit on the
    -- ring at the slice's middle, for the slices with room for one.
    starts = scanl (+) 0 (map shareOf slices)
    -- (each slice: its colour, the slice, its share as text, the dash and
    -- the offset of its circle, its label's place, and whether it has room
    -- for a label)
    ring = [ (fromMaybe (i `mod` 8) (sColour s), s, pctText pct, dash pct, offset start, lx, ly, pct >= 7)
           | (i, s, start) <- zip3 [0 :: Int ..] slices starts, let pct = shareOf s, let (lx, ly) = labelAt start pct ]
    dash pct = T.pack (printf "%.3f %.3f" (realToFrac pct :: Double) (100 - realToFrac pct :: Double))
    offset start = T.pack (printf "%.3f" (25 - realToFrac start :: Double))
    labelAt start pct = (T.pack (printf "%.2f" x), T.pack (printf "%.2f" y))
      where
        angle = (realToFrac (start + pct / 2) :: Double) * 2 * pi / 100
        x = 21 + 15.9 * sin angle
        y = 21 - 15.9 * cos angle
    pctText pct = T.pack (printf "%.1f" (realToFrac pct :: Double))
    -- What the page is about, and the way back up: Breakdown › Expense › food
    path = [(tr trs "Breakdown", link [])]
        ++ [(kindLabel k, link [("kind", kindKey k)]) | Just k <- [mkind]]
        ++ [ (part, link ([("kind", kindKey k) | Just k <- [mkind]] ++ [("acct", T.intercalate ":" (take n parts))]))
           | Just a <- [macct], let parts = accountNameComponents a, (n, part) <- zip [2 ..] (drop 1 parts) ]
    -- (each step: whether it is the first, whether the last, its name, its link)
    pathRows = [ (k == 1, k == length path, name, target) | (k, (name, target)) <- zip [1 :: Int ..] path ]
    centre = case (mkind, macct) of
      (_, Just a) -> accountLeafName a
      (Just k, _) -> kindLabel k
      _ -> ""
    centreShort = if T.length centre > 11 then T.take 10 centre <> "\8230" else centre
    grandAmount = styleAmounts styles $ maSum (map sAmount slices)
    entriesLink = case macct of
      Just a -> Just (registerLink a)
      Nothing -> Nothing
    noBelow = const mempty
  defaultLayout $ do
    -- TRANSLATORS: the browser tab title of the breakdown page.
    setTitleI (HMsg "breakdown - hledger-web")
    $(widgetFile "breakdown")
