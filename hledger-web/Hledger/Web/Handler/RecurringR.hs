-- | The recurring rules: the periodic transactions the journal declares,
-- from which entries are written ahead. A page lists them, with each
-- rule's next date; a rule's own page is the add form with a schedule
-- above it, to change the rule in place in its file, or delete it; a new
-- rule is appended to the file the rules are in.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Web.Handler.RecurringR
  ( getRecurringR
  , getRecurringNewR
  , postRecurringNewR
  , getRecurringRuleR
  , postRecurringRuleR
  ) where

import Data.List (dropWhileEnd)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Text.Read (readMaybe)

import Data.Time.Calendar (addGregorianMonthsClip, fromGregorian, toGregorian)

import Hledger
import Hledger.Utils.I18n (Translations, tr, trf)
import Hledger.Web.Import
import Hledger.Web.WebOptions (WebOpts(..))
import Hledger.Web.Widget.AddForm (addFormForRule, transactionCollapseNewlines)
import Hledger.Web.Widget.Common (accountNameAsHtml, writeJournalTextIfValidAndChanged)

-- | The schedules the form offers: a key, a label, the interval, and the
-- period expression's word for it. Anything else is written as a period
-- expression.
schedules :: Translations -> [(Text, Text, Interval, Text)]
schedules trs =
  [ ("weekly",    tr trs "Every week",     Weeks 1,    "weekly")
  , ("biweekly",  tr trs "Every 2 weeks",  Weeks 2,    "every 2 weeks")
  , ("monthly",   tr trs "Every month",    Months 1,   "monthly")
  , ("bimonthly", tr trs "Every 2 months", Months 2,   "every 2 months")
  , ("quarterly", tr trs "Every quarter",  Quarters 1, "quarterly")
  , ("yearly",    tr trs "Every year",     Years 1,    "yearly")
  ]

-- | The schedule the list offers beyond hledger's single rhythms: twice
-- a month, on the 1st and the 15th, which is two monthly rules.
twiceKey :: Text
twiceKey = "twice"

twiceLabel :: Translations -> Text
twiceLabel trs = tr trs "Twice a month, on the 1st and the 15th"

-- | The first 1st, and the first 15th, on or after a date.
firstAndFifteenth :: Day -> (Day, Day)
firstAndFifteenth d = (onOrAfter 1, onOrAfter 15)
  where
    (y, m, dd) = toGregorian d
    onOrAfter n = if dd <= n then fromGregorian y m n else addGregorianMonthsClip 1 (fromGregorian y m n)

-- | One past the highest rule number in use.
nextNumber :: Journal -> Int
nextNumber j = 1 + maximum (0 : [n | pt <- jperiodictxns j, (k, v) <- pttags pt, k == "recurring", Just n <- [readMaybe (T.unpack v) :: Maybe Int]])

scheduleKey :: Translations -> Interval -> Maybe Text
scheduleKey trs ivl = listToMaybe [k | (k, _, i, _) <- schedules trs, i == ivl]

scheduleWord :: Translations -> Text -> Maybe Text
scheduleWord trs key = listToMaybe [w | (k, _, _, w) <- schedules trs, k == key]

scheduleLabel :: Translations -> PeriodicTransaction -> Text
scheduleLabel trs pt = case scheduleKey trs (ptinterval pt) of
  Just k | Just l <- listToMaybe [l | (k', l, _, _) <- schedules trs, k' == k] -> l
  _ -> ptperiodexpr pt

-- | The rules, numbered from one, as the pages name them.
rules :: Journal -> [(Int, PeriodicTransaction)]
rules j = zip [1 ..] (jperiodictxns j)

-- | Where a rule's text is: its file and its lines, first and last.
ruleSpan :: PeriodicTransaction -> (FilePath, Int, Int)
ruleSpan PeriodicTransaction{ptsourcepos = (startpos, endpos)} =
  (sourceName startpos, unPos (sourceLine startpos), unPos (sourceLine endpos) - 1)

-- | A rule's next date from today, if it has one in the coming year.
nextDate :: Day -> PeriodicTransaction -> Maybe Day
nextDate today pt =
  case runPeriodicTransaction False pt (DateSpan (Just (Exact today)) (Just (Exact (addDays 366 today)))) of
    t : _ -> Just (tdate t)
    [] -> Nothing

-- | A rule as a transaction, for the add form: dated on its start.
asTransaction :: Day -> PeriodicTransaction -> Transaction
asTransaction today pt = nulltransaction
  { tdate = fromMaybe today (spanStart (ptspan pt))
  , tdescription = ptdescription pt
  , tpostings = ptpostings pt
  }

-- | The add form with a schedule above it. What it gives: the entry the
-- form made, the schedule's key, a period expression written instead,
-- and an end date.
recurringForm :: Translations -> Journal -> Day -> Maybe PeriodicTransaction
              -> Markup -> MForm Handler (FormResult (Transaction, Text, Maybe Text, Maybe Day), Widget)
recurringForm trs j today mpt extra = do
  (txnRes, txnView) <- addFormForRule j today (asTransaction today <$> mpt) extra
  let defKey = fromMaybe "monthly" (mpt >>= scheduleKey trs . ptinterval)
      -- a rule whose interval the list lacks keeps its expression
      defExpr = case mpt of
        Just pt | isNothing (scheduleKey trs (ptinterval pt)) -> Just (ptperiodexpr pt)
        _ -> Nothing
      defUntil = mpt >>= spanEnd . ptspan
      schedSettings = FieldSettings "schedule" Nothing Nothing (Just "schedule") [("class", "form-control input-lg")]
      exprSettings = FieldSettings "periodexpr" Nothing Nothing (Just "periodexpr") [("class", "form-control input-lg"), ("placeholder", tr trs "or a period expression, such as: every 3 months from 2026-01-15")]
      untilSettings = FieldSettings "until" Nothing Nothing (Just "until") [("class", "form-control input-lg")]
  (schedRes, schedView) <- mreq (selectFieldList ([(l, k) | (k, l, _, _) <- schedules trs] ++ [(twiceLabel trs, twiceKey)])) schedSettings (Just defKey)
  (exprRes, exprView) <- mopt textField exprSettings (Just defExpr)
  (untilRes, untilView) <- mopt dayField untilSettings (Just defUntil)
  let res = (\(t, _) k e u -> (t, k, mfilter (not . T.null . T.strip) e, u)) <$> txnRes <*> schedRes <*> exprRes <*> untilRes
      widget = $(widgetFile "recurring-form")
  return (res, widget)

getRecurringR :: Handler Html
getRecurringR = do
  checkServerSideUiEnabled
  VD{j, today, trs} <- getViewData
  require AddPermission
  let rows = [ (i, pt, scheduleLabel trs pt, spanStart (ptspan pt), spanEnd (ptspan pt), nextDate today pt
               , payeeAndNote' pt, ruleAmount pt, ruleAccounts pt)
             | (i, pt) <- rules j ]
      payeeAndNote' pt = payeeAndNoteFromDescription (ptdescription pt)
      -- the amount the rule names: its first posting's, as the form shows it
      ruleAmount pt = case [pamount p | p <- ptpostings pt, not (mixedAmountLooksZero (pamount p)), pamount p /= missingmixedamt] of
        a : _ -> wbToText (showMixedAmountB oneLineNoCostFmt a)
        [] -> ""
      ruleAccounts pt = zip [1 :: Int ..] (map paccount (ptpostings pt))
  defaultLayout $ do
    setTitleI (HMsg "recurring rules - hledger-web")
    $(widgetFile "recurring")

getRecurringNewR :: Handler Html
getRecurringNewR = do
  checkServerSideUiEnabled
  VD{j, today, trs} <- getViewData
  require AddPermission
  (view, enctype) <- generateFormPost (recurringForm trs j today Nothing)
  rulePage trs Nothing "" view enctype

getRecurringRuleR :: Int -> Handler Html
getRecurringRuleR i = do
  checkServerSideUiEnabled
  VD{j, today, trs} <- getViewData
  require AddPermission
  (pt, _, _, _, original) <- lookupRule j i
  (view, enctype) <- generateFormPost (recurringForm trs j today (Just pt))
  rulePage trs (Just (i, pt)) original view enctype

-- | The rule with this number, and its text as the file has it now.
lookupRule :: Journal -> Int -> Handler (PeriodicTransaction, FilePath, Int, Int, Text)
lookupRule j i = do
  pt <- maybe notFound pure $ lookup i (rules j)
  let (f, l1, l2) = ruleSpan pt
  when (null f || f `notElem` journalFilePaths j) notFound
  ls <- liftIO $ T.lines <$> T.readFile f
  pure (pt, f, l1, l2, T.unlines $ take (l2 - l1 + 1) $ drop (l1 - 1) ls)

rulePage :: Translations -> Maybe (Int, PeriodicTransaction) -> Text -> Widget -> Enctype -> Handler Html
rulePage trs mrule original view enctype = do
  mtoken <- reqToken <$> getRequest
  let (action, heading, editing) = case mrule of
        Just (i, pt) -> (RecurringRuleR i, scheduleLabel trs pt <> ": " <> ptdescription pt, True)
        Nothing -> (RecurringNewR, tr trs "New recurring rule", False)
  defaultLayout $ do
    setTitleI (HMsg "recurring rule - hledger-web")
    $(widgetFile "recurring-rule")

postRecurringNewR :: Handler Html
postRecurringNewR = do
  checkServerSideUiEnabled
  VD{j, today, trs, opts} <- getViewData
  require AddPermission
  ((res, view), enctype) <- runFormPost $ recurringForm trs j today Nothing
  case res of
    FormSuccess made -> do
      when (showroom_ opts) $ do
        setMessageI (HMsg "This is a showroom: nothing is saved.")
        redirect RecurringR
      -- numbers of its own: one past the highest in use (two, twice a month)
      let comments = ["recurring:" <> T.pack (show n) | n <- [nextNumber j ..]]
      case ruleTexts trs today comments made of
        Left err -> setMessage (toHtml err) >> rulePage trs Nothing "" view enctype
        Right txts -> do
          -- appended to the file the rules are in, else the main file
          let f = case jperiodictxns j of
                pt : _ -> sourceName (fst (ptsourcepos pt))
                [] -> journalFilePath j
          ls <- liftIO $ T.lines <$> T.readFile f
          saveLines trs f (T.unlines (dropWhileEnd T.null ls ++ concat [ "" : T.lines txt | txt <- txts ])) RecurringNewR
          setMessageI (HMsg "Rule saved.")
          redirect RecurringR
    FormMissing -> rulePage trs Nothing "" view enctype
    FormFailure errs -> do
      setMessage $ toHtml $ T.intercalate "; " errs
      rulePage trs Nothing "" view enctype

postRecurringRuleR :: Int -> Handler Html
postRecurringRuleR i = do
  checkServerSideUiEnabled
  VD{j, today, trs, opts} <- getViewData
  require AddPermission
  (pt, f, l1, l2, current) <- lookupRule j i
  original <- maybe "" (T.replace "\r\n" "\n") <$> lookupPostParam "original"
  action <- fromMaybe "save" <$> lookupPostParam "action"
  token <- lookupPostParam "_token"
  sessionToken <- reqToken <$> getRequest
  when (showroom_ opts) $ do
    setMessageI (HMsg "This is a showroom: nothing is saved.")
    redirect (RecurringRuleR i)
  if original /= current
    then do
      setMessage $ toHtml $ tr trs "This rule changed since the page was opened. It is shown as it is now; make the change again."
      redirect (RecurringRuleR i)
    else if action == "delete"
      then do
        when (isNothing sessionToken || token /= sessionToken) $ permissionDenied "CSRF token mismatch"
        ls <- liftIO $ T.lines <$> T.readFile f
        let rest = dropWhile T.null (drop l2 ls)
            before = (if null rest then dropWhileEnd T.null else id) (take (l1 - 1) ls)
        saveLines trs f (T.unlines (before ++ rest)) (RecurringRuleR i)
        setMessageI (HMsg "Rule deleted.")
        redirect RecurringR
      else do
        ((res, view), enctype) <- runFormPost $ recurringForm trs j today (Just pt)
        case res of
          FormSuccess made ->
            -- the rule's own comment, with its number, stays; a second
            -- rule (twice a month) gets a number of its own
            case ruleTexts trs today (T.strip (ptcomment pt) : ["recurring:" <> T.pack (show n) | n <- [nextNumber j ..]]) made of
              Left err -> setMessage (toHtml err) >> rulePage trs (Just (i, pt)) current view enctype
              Right txts -> do
                ls <- liftIO $ T.lines <$> T.readFile f
                saveLines trs f (T.unlines (take (l1 - 1) ls ++ T.lines (T.intercalate "\n" txts) ++ drop l2 ls)) (RecurringRuleR i)
                setMessageI (HMsg "Rule saved.")
                redirect RecurringR
          FormMissing -> rulePage trs (Just (i, pt)) current view enctype
          FormFailure errs -> do
            setMessage $ toHtml $ T.intercalate "; " errs
            rulePage trs (Just (i, pt)) current view enctype

-- | The rules' texts from what the form made: one, or two for twice a
-- month (monthly from the first 1st, and from the first 15th, on or after
-- the entry's date), each with its own comment; or what is wrong.
ruleTexts :: Translations -> Day -> [Text] -> (Transaction, Text, Maybe Text, Maybe Day) -> Either Text [Text]
ruleTexts trs today comments made@(t, key, mexpr, muntil)
  | key == twiceKey && isNothing mexpr =
      let (d1, d15) = firstAndFifteenth (tdate t)
      in sequence [ ruleText trs today c (t{tdate = d}, "monthly", Nothing, muntil) | (c, d) <- zip comments [d1, d15] ]
  | otherwise = (: []) <$> ruleText trs today (fromMaybe "" (listToMaybe comments)) made

-- | A rule's text from what the form made: the period expression (the
-- schedule from the entry's date, or the one written, to the end date if
-- any), the description, the comment, and the postings as an entry has
-- them; or what is wrong with the period.
ruleText :: Translations -> Day -> Text -> (Transaction, Text, Maybe Text, Maybe Day) -> Either Text Text
ruleText trs today comment (t, key, mexpr, muntil) = do
  let expr = case mexpr of
        Just e -> T.strip e
        Nothing -> fromMaybe "monthly" (scheduleWord trs key) <> " from " <> showDate (tdate t)
                   <> maybe "" ((" to " <>) . showDate) muntil
  -- (not hledger's check that the start falls on a period boundary: its
  -- forecast runs "monthly from the 28th" on the 28th, which is the point)
  _ <- either (const $ Left $ trf trs "Could not read the period: {period}" [("period", expr)]) Right $ parsePeriodExpr today expr
  let t' = txnTieKnot $ transactionCollapseNewlines t
      shown = T.lines (showTransaction t')
      first = "~ " <> expr <> (if T.null (tdescription t') then "" else "  " <> tdescription t') <> (if T.null comment then "" else "  ; " <> comment)
  Right $ T.unlines (first : drop 1 shown)

-- | Write a file's new text, if the journal still reads; else say why
-- and go back to the page.
saveLines :: Translations -> FilePath -> Text -> AppRoute -> Handler ()
saveLines trs f txt back =
  runExceptT (writeJournalTextIfValidAndChanged f txt) >>= \case
    Left e -> do
      setMessage $ toHtml $ trf trs "Failed to load journal: {error}" [("error", T.pack e)]
      redirect back
    Right () -> pure ()
