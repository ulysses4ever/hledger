-- | A transaction's own page: a card with what it was, Edit and Delete,
-- and on Edit the add form filled with it. Changes are written back to
-- the journal file the transaction came from, in place of its text there.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Web.Handler.TransactionR
  ( getTransactionR
  , postTransactionR
  ) where

import Data.List (dropWhileEnd)
import Data.Text qualified as T
import Data.Text.IO qualified as T

import Hledger
import Hledger.Utils.I18n (tr, trc, trf)
import Hledger.Web.Import
import Hledger.Web.WebOptions (WebOpts(..))
import Hledger.Web.Widget.AddForm (addForm, transactionCollapseNewlines)
import Hledger.Web.Widget.Common (accountNameAsHtml, accountQuery, writeJournalTextIfValidAndChanged)
import Hledger.Web.Widget.Entry (EntryKind(..), EntrySummary(..), summarizeEntry, entryKindClass, dayWithWeekday)

-- | Where a transaction's text is: its file and its lines, first and last (1-based, inclusive).
transactionSpan :: Transaction -> (FilePath, Int, Int)
transactionSpan Transaction{tsourcepos = (startpos, endpos)} =
  (sourceName startpos, unPos (sourceLine startpos), unPos (sourceLine endpos) - 1)

-- | The transaction with this index, and its text as the file has it now.
-- A transaction the journal generated (a forecast) has no file and no page.
lookupTransaction :: Journal -> Integer -> Handler (Transaction, FilePath, Int, Int, Text)
lookupTransaction j i = do
  t <- maybe notFound pure $ find ((== i) . tindex) (jtxns j)
  let (f, l1, l2) = transactionSpan t
  when (null f || f `notElem` journalFilePaths j) notFound
  ls <- liftIO $ T.lines <$> T.readFile f
  pure (t, f, l1, l2, T.unlines $ take (l2 - l1 + 1) $ drop (l1 - 1) ls)

getTransactionR :: Integer -> Handler Html
getTransactionR i = do
  checkServerSideUiEnabled
  VD{j, today} <- getViewData
  require AddPermission
  (t, _, _, _, original) <- lookupTransaction j i
  (view, enctype) <- generateFormPost (addForm j today (Just t))
  -- the form shows when Edit asks for it
  editing <- (== Just "1") <$> lookupGetParam "edit"
  showPage t original view enctype editing

showPage :: Transaction -> Text -> Widget -> Enctype -> Bool -> Handler Html
showPage t original view enctype editing = do
  -- the delete form is not a yesod form, so it carries the session's CSRF
  -- token itself, checked in postTransactionR
  mtoken <- reqToken <$> getRequest
  VD{j, trs} <- getViewData
  -- an instance of a recurring rule says so, and links to the rule
  let mrule = case lookup "recurring" (ttags t) of
        Just n -> listToMaybe [ (i, ptperiodexpr pt) | (i, pt) <- zip [1 :: Int ..] (jperiodictxns j), lookup "recurring" (pttags pt) == Just n ]
        Nothing -> Nothing
      -- The card: the entry as the journal's blocks sum it up (how much,
      -- what for), its date, and under them its facts: the account it was
      -- paid from or into (both, for a transfer), its payee and its note.
      es = summarizeEntry (journalAccountType j) t
      kindClass = entryKindClass (esKind es)
      oneLine = wbToText . showMixedAmountB oneLineNoCostFmt{displayZeroCommodity = True}
      -- (a description without a pipe is all payee, not all note)
      (payee, note) = case payeeAndNoteFromDescription' (tdescription t) of
        ("", whole) -> (whole, "")
        pn -> pn
      facts :: [(Text, Either AccountName Text)]
      facts =
        (case esKind es of
           Moving   -> [(tr trs "From", Left a) | a <- esFrom es] ++ [(tr trs "To", Left a) | a <- esTo es]
           Spending -> [(tr trs "Account", Left a) | a <- esFrom es]
           Earning  -> [(tr trs "Account", Left a) | a <- esTo es])
        ++ [(trc trs "placeholder" "Payee", Right payee) | not (T.null payee)]
        ++ [(trc trs "placeholder" "Note", Right note) | not (T.null note)]
      editLink = (TransactionR (tindex t), [("edit", "1")])
      back = (JournalR, [("q", "date:" <> T.pack (show (tdate t)))])
  defaultLayout $ do
    setTitleI (HMsg "transaction - hledger-web")
    $(widgetFile "transaction")

postTransactionR :: Integer -> Handler Html
postTransactionR i = do
  checkServerSideUiEnabled
  VD{j, today, trs, opts} <- getViewData
  require AddPermission
  (t, f, l1, l2, current) <- lookupTransaction j i
  -- a showroom shows the page, and saves nothing
  when (showroom_ opts) $ do
    setMessageI (HMsg "This is a showroom: nothing is saved.")
    redirect (TransactionR i)
  -- (a browser sends the hidden field's newlines as CRLF)
  original <- maybe "" (T.replace "\r\n" "\n") <$> lookupPostParam "original"
  action <- fromMaybe "save" <$> lookupPostParam "action"
  token <- lookupPostParam "_token"
  sessionToken <- reqToken <$> getRequest
  -- The page was made from the text as it was then; if the file has changed
  -- since, the change would land on the wrong lines, so refuse.
  if original /= current
    then do
      setMessage $ toHtml $ tr trs "This entry changed since the page was opened. It is shown as it is now; make the change again."
      redirect (TransactionR i)
    else if action == "delete"
      then do
        when (isNothing sessionToken || token /= sessionToken) $ permissionDenied "CSRF token mismatch"
        write f l1 l2 ""
        setMessageI (HMsg "Transaction deleted.")
        redirect (JournalR, [("q", "date:" <> T.pack (show (tdate t)))])
      else do
        ((res, view), enctype) <- runFormPost $ addForm j today (Just t)
        case res of
          FormSuccess (t', _) -> do
            -- what the form does not show stays as it was
            let t'' = txnTieKnot $ transactionCollapseNewlines t'{tcode = tcode t, tcomment = tcomment t, ttags = ttags t, tprecedingcomment = tprecedingcomment t}
            write f l1 l2 (showTransaction t'')
            setMessageI (HMsg "Transaction saved.")
            redirect (JournalR, [("q", "date:" <> T.pack (show (tdate t'')))])
          FormMissing -> showPage t current view enctype True
          FormFailure errs -> do
            setMessage $ toHtml $ T.intercalate "; " errs
            showPage t current view enctype True
  where
    -- Replace the lines l1..l2 of the file with the text (a transaction, or
    -- nothing), keeping the file parseable: the write is refused otherwise.
    write f l1 l2 new = do
      ls <- liftIO $ T.lines <$> T.readFile f
      let rest = drop l2 ls
          -- a deleted entry takes a blank line that separated it with it:
          -- the one after it, or when it was the last, the one before it
          rest' = if T.null new then dropWhile T.null rest else rest
          before = (if T.null new && null rest' then dropWhileEnd T.null else id) $ take (l1 - 1) ls
          txt = T.unlines $ before ++ (if T.null new then [] else T.lines new) ++ rest'
      runExceptT (writeJournalTextIfValidAndChanged f txt) >>= \case
        Left e -> do
          VD{trs} <- getViewData
          setMessage $ toHtml $ trf trs "Failed to load journal: {error}" [("error", T.pack e)]
          redirect (TransactionR i)
        Right () -> pure ()
