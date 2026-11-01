{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Web.Widget.AddForm
  ( addForm
  , addModal
  , transactionCollapseNewlines
  ) where

import Control.Applicative ((<|>))
import Control.Monad.State.Strict (evalStateT)
import Data.Bifunctor (first)
import Data.Char (isAlpha)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (dropWhileEnd, elemIndex, groupBy, maximumBy, nub, sortOn, unfoldr)
import Data.Map.Strict qualified as M
import Data.Maybe (isJust, fromMaybe, listToMaybe)
import Data.Ord (comparing)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day)
import Text.Blaze.Internal (Markup)
import Text.Megaparsec (bundleErrors, eof, parseErrorTextPretty, runParser)
import Yesod

import Hledger.Utils.I18n (Translations, substitutePlaceholders, tr, trc, trf)
import Hledger
import Hledger.Web.App (App, Handler, Widget, HMsg(..), requestTranslations)
import Hledger.Web.Settings (widgetFile)
import Data.Function ((&))
import Control.Arrow (right)

-- | Replace newlines with spaces in the transaction fields which are written
-- to the journal file verbatim: the description, the code, and the posting
-- account names. A newline in one of these would split the rendered entry
-- across lines, letting arbitrary journal directives (eg an include) be
-- written into the file. The journal format can't represent a newline in
-- these fields anyway - their parsers stop at end of line - and the CSV
-- reader collapses them likewise, so nothing that could round trip is lost.
-- Comments are left alone: they are rendered as one ";" line per line, so
-- newlines in them are safe.
transactionCollapseNewlines :: Transaction -> Transaction
transactionCollapseNewlines t = t
  { tdescription = collapse $ tdescription t
  , tcode        = collapse $ tcode t
  , tpostings    = map collapseacct $ tpostings t
  }
  where
    collapse = T.map (\c -> if c == '\n' || c == '\r' then ' ' else c)
    -- Account names are also normalised to single spaces, since two spaces
    -- would end the account name when the entry is read back.
    collapseacct p = p{paccount = T.unwords . T.words $ paccount p}

addModal :: Route App -> Journal -> Day -> Widget
addModal addR j today = do
  (addView, addEnctype) <- handlerToWidget $ generateFormPost (addForm j today Nothing)
  [whamlet|
<dialog #addmodal aria-labelledby="addLabel">
  <div .modal-header>
    <button type="button" .close data-dismiss="modal" aria-label=_{HMsg "Close"}>&times;
    <h3 .modal-title #addLabel>_{HMsg "Add a transaction"}
  <div .modal-body>
    <form#addform.form action=@{addR} method=POST enctype=#{addEnctype}>
      ^{addView}
|]

-- | The form for a transaction: empty for a new one, or filled with an
-- existing one for editing it (the transaction page). Edited, the
-- transaction keeps its code, comment and tags, which the form does not
-- show; its status comes from the form.
addForm :: Journal -> Day -> Maybe Transaction -> Markup -> MForm Handler (FormResult (Transaction,FilePath), Widget)
addForm j today mtxn = identifyForm "add" $ \extra -> do
  trs <- lift requestTranslations
  let  -- bindings used in add-form.hamlet
    editing = isJust mtxn
    -- the existing transaction's fields, as the form shows them
    (defPayee, defNote) = case T.breakOn " | " . tdescription <$> mtxn of
      Just (p, n) | not (T.null n) -> (Just (T.strip p), Just (T.strip (T.drop 3 n)))
      Just (p, _) -> (Just (T.strip p), Nothing)
      Nothing -> (Nothing, Nothing)
    -- each posting's account, amount and currency; the last amount is left
    -- to hledger, as in a new entry, so that changing the first amount keeps
    -- the entry balanced
    defRows = case tpostings <$> mtxn of
      Just ps@(_ : _) -> [(paccount p, if i == length ps then "" else shownNumber p, shownCommodity p) | (i, p) <- zip [1 :: Int ..] ps]
      _ -> []
    -- an amount as it is typed: the number alone, its currency picked beside
    -- it, and an income's positive (see validatePostings)
    shownNumber p = case amountsRaw (pamount p) of
      [a] -> wbToText $ showAmountB machineFmt{displayCommodity = False} (if isRevenue (paccount p) then negate a else a)
      _ -> wbToText $ showMixedAmountB oneLineNoCostFmt (pamount p)
    shownCommodity p = case amountsRaw (pamount p) of
      [a] -> acommodity a
      _ -> ""
    isRevenue a = accountType a == Just Revenue
    -- the currencies to pick from beside an amount: the journal's, dollars first
    commodities = filter (not . T.null) $ S.toList $ journalCommodities j
    defaultCommodity = if "$" `elem` commodities then "$" else fromMaybe "" (listToMaybe commodities)
    pickedCommodity c = if T.null c then defaultCommodity else c
    commodityLabel c = fromMaybe c $ lookup c [("$", "USD"), ("\8364", "EUR"), ("\163", "GBP"), ("\8381", "RUB"), ("\165", "JPY")]
    statusOptions = [(tr trs "Unmarked", Unmarked), (tr trs "Pending", Pending), (tr trs "Cleared", Cleared)]
    descriptions = foldMap S.fromList [journalPayeesDeclaredOrUsed j, journalDescriptions j]
    -- the payees to pick from: the ten used most, then all of them
    payeeCounts = M.fromListWith (+) [(transactionPayee t, 1 :: Int) | t <- jtxns j, not (T.null (transactionPayee t))]
    frequentPayees = map fst $ take 10 $ sortOn (negate . snd) $ M.toList payeeCounts
    allPayees = sortOn T.toLower $ filter (not . T.null) $ journalPayeesDeclaredOrUsed j
    payeePlaceholder = trc trs "placeholder" "Payee"
    files = fst <$> jfiles j
    deffile = journalFilePath j
    accountPlaceholder = tr trs "Account {n}"
    amountPlaceholder = tr trs "Amount {n}"
    numbered template n = substitutePlaceholders [("n", T.pack (show n))] template
    -- The second row is the account the entry is paid from or into; later
    -- rows are numbered.
    accountLabel :: Int -> Text
    accountLabel 2 = tr trs "Account"
    accountLabel n = numbered accountPlaceholder n
    amountLabel :: Int -> Text
    amountLabel 1 = tr trs "Amount"
    amountLabel n = numbered amountPlaceholder n
    -- The first row picks a category, then an account within it: under
    -- Expenses and Income, each second-level account (food, utilities, ...)
    -- is a category whose accounts are listed by their remaining parts; the
    -- other top-level accounts are each one category, under "Transfer".
    -- Each category: its key (an account name), its label, and its accounts.
    isCategoryTop t = T.toLower t `elem` ["expenses", "income", "revenues"]
    categoryKey a = case accountNameComponents a of
      t : c : _ | isCategoryTop t -> t <> ":" <> c
      t : _ -> t
      [] -> a
    categoryLabel k = case accountNameComponents k of
      _ : c : _ -> c
      t : _ -> T.toTitle t
      [] -> k
    withinLabel k a = case drop (length (accountNameComponents k)) (accountNameComponents a) of
      [] -> categoryLabel k
      rest -> T.intercalate " \8250 " rest
    names = journalAccountNamesDeclaredOrImplied j
    -- A category's accounts are those under it; one with none is itself the account.
    within k = case [(a, withinLabel k a) | a <- names, categoryKey a == k, a /= k, not (isCategoryTop a)] of
      [] -> [(k, categoryLabel k)]
      subs -> subs
    categoryKeys = nub $ map categoryKey [a | a <- names, not (isCategoryTop a)]
    -- Each group: its key (the kind of entry it is), its label, its categories.
    categoryGroups :: [(Text, Text, [(Text, Text, [(Text, Text)])])]
    categoryGroups =
      [ (key, grp, cats)
      | (key, grp, isIn) <- [("expenses", tr trs "Expenses", (== "expenses") . T.toLower . topOf), ("income", tr trs "Income", (`elem` ["income", "revenues"]) . T.toLower . topOf), ("transfer", tr trs "Transfer", not . isCategoryTop . topOf)]
      , let cats = [ (k, categoryLabel k, within k) | k <- categoryKeys, isIn k ]
      , not (null cats) ]
    -- The accounts to pick from, grouped by top-level account, each shown
    -- without that top level: under "Expenses", "food › groceries". The
    -- first row is usually the expense or income, the others the account
    -- that pays or receives, so each row lists its likely groups first;
    -- every group is in every row, since a transfer names two accounts.
    topOf = T.takeWhile (/= ':')
    accountGroups :: [(Text, [(Text, Text)])]
    accountGroups =
      [ (T.toTitle (topOf (head g)), [(a, optionLabel a) | a <- g])
      | g <- groupBy ((==) `on` topOf) (journalAccountNamesDeclaredOrImplied j) ]
    accountGroupsFor :: Int -> [(Text, [(Text, Text)])]
    accountGroupsFor n = sortOn (rank . fst) accountGroups
      where
        first = if n == 1 then ["expenses", "income", "revenues"] else ["assets", "liabilities", "equity"]
        rank g = fromMaybe (length first) $ elemIndex (T.toLower g) first
    optionLabel a = case accountNameComponents a of
      _ : rest@(_ : _) -> T.intercalate " \8250 " rest
      _ -> a
    -- The second row's default: the asset or liability account with the most
    -- postings, which is what most entries are paid from. An account's type is
    -- the journal's, else what its name says (Assets, liabilities:card, ...).
    accountType a = journalAccountType j a <|> accountNameInferType a
    fundingDefault =
      case M.toList $ M.fromListWith (+) [(paccount p, 1 :: Int) | p <- journalPostings j, accountType (paccount p) `elem` [Just Asset, Just Cash, Just Liability]] of
        [] -> ""
        counts -> fst $ maximumBy (comparing snd) counts
    -- The account a row shows: what was submitted, else the row's default.
    chosen :: Int -> Text -> Text
    chosen n acc
      | not (T.null acc) = acc
      | n == 2 = fundingDefault
      | otherwise = ""
    -- custom fields
    dateField = textField & checkMMap (pure . right fromEFDay . validateDate) (T.pack . show)
      where
        validateDate s =
          first (const (tr trs "Invalid date format")) $
          fixSmartDateStrEither' today (T.strip s)
    dateSettings = FieldSettings "date" Nothing Nothing (Just "date") [("class", "form-control input-lg"), ("placeholder", trc trs "placeholder" "Date")]
    descSettings = FieldSettings "desc" Nothing Nothing (Just "description") [("class", "form-control input-lg"), ("placeholder", trc trs "placeholder" "Payee"), ("size", "40"), ("list", "descriptionnames")]
    noteSettings = FieldSettings "note" Nothing Nothing (Just "note") [("class", "form-control input-lg"), ("placeholder", trc trs "placeholder" "Note"), ("size", "40")]
    statusSettings = FieldSettings "status" Nothing Nothing (Just "status") [("class", "form-control input-lg")]
  (dateRes, dateView) <- mreq dateField dateSettings (tdate <$> mtxn)
  (descRes, descView) <- mopt textField descSettings (Just defPayee)
  (noteRes, noteView) <- mopt textField noteSettings (Just defNote)
  -- optional: a client that does not send it (the JSON API, an old form)
  -- leaves the status as it was, unmarked for a new entry
  (statusRes, statusView) <- mopt (selectFieldList statusOptions) statusSettings (Just . tstatus <$> mtxn)
  (acctsRes, _)       <- mreq listField acctSettings Nothing
  (amtsRes, _)        <- mreq listField amtSettings  Nothing
  (cmdtysRes, _)      <- mreq listField cmdtySettings Nothing
  (fileRes, fileView) <- mopt fileField' fileSettings Nothing
  let
    (postingsRes, displayRows) = validatePostings trs isRevenue defRows acctsRes amtsRes cmdtysRes
    -- hledger keeps a note after the payee in the description: "Payee | note".
    descWithNote = joinNote <$> descRes <*> noteRes
    joinNote mpayee mnote = case (T.strip <$> mpayee, T.strip <$> mnote) of
      (payee, Just note) | not (T.null note) -> Just (fromMaybe "" payee <> " | " <> note)
      (payee, _) -> payee
    formRes = withStatus <$> statusRes <*> validateTransaction deffile dateRes descWithNote postingsRes fileRes
    withStatus mst (t, f) = (t{tstatus = fromMaybe (maybe Unmarked tstatus mtxn) mst}, f)
  return (formRes, $(widgetFile "add-form"))
  where
    listField = Field
      { fieldParse = const . pure . Right . Just . dropWhileEnd T.null
      , fieldView = error' "listField should not be used for rendering"  -- PARTIAL:
      , fieldEnctype = UrlEncoded
      }
    fileField' :: Field Handler FilePath
    fileField' = selectFieldList [(T.pack f, f) | f <- fs] & check validateFilepath
      where
        fs = journalFilePaths j
        validateFilepath :: FilePath -> Either FormMessage FilePath
        validateFilepath f
          | f `elem` fs = Right f
          | otherwise = Left $ MsgInputNotFound $ T.pack f
    -- field settings
    acctSettings = FieldSettings "account" Nothing Nothing (Just "account") []
    amtSettings  = FieldSettings "amount" Nothing Nothing (Just "amount") []
    cmdtySettings = FieldSettings "commodity" Nothing Nothing (Just "commodity") []
    fileSettings = FieldSettings "file" Nothing Nothing (Just "file") [("class", "form-control input-lg")]

validateTransaction ::
     FilePath -> FormResult Day -> FormResult (Maybe Text) -> FormResult [Posting] -> FormResult (Maybe FilePath)
  -> FormResult (Transaction, FilePath)
validateTransaction deffile dateRes descRes postingsRes fileRes =
  case makeTransaction <$> dateRes <*> descRes <*> postingsRes <*> fileRes of
    FormSuccess (txn,f) -> case balanceSingleTransaction defbalancingopts txn of
      Left e     -> FormFailure [T.pack e]
      Right txn' -> FormSuccess (txn',f)
    x -> x
  where
    makeTransaction date mdesc postings mfile =
      (nulltransaction {
         tdate = date
        ,tdescription = fromMaybe "" mdesc
        ,tpostings = postings
        ,tsourcepos = (initialPos f, initialPos f)
        }, f)
      where f = fromMaybe deffile mfile

-- | Parse a list of postings out of a list of accounts and a corresponding list
-- of amounts, each with the currency picked beside it. An amount typed on a
-- revenue account is money received, so it is stored negated, as the books
-- have it.
validatePostings ::
     Translations -> (Text -> Bool) -> [(Text, Text, Text)] -> FormResult [Text] -> FormResult [Text] -> FormResult [Text]
  -> (FormResult [Posting], [(Int, (Text, Text, Text, Maybe Text, Maybe Text))])
validatePostings trs isRevenue defaults acctsRes amtsRes cmdtysRes = let
  missingamount = tr trs "Missing amount"
  missingaccount = tr trs "Missing account"

  -- Zip accounts, amounts and currencies, fill in missing values and drop empty rows.
  rows :: [(Text, Text, Text)]
  rows = [ (acc, amt, c) | ((acc, amt), c) <- zip pairs (formSuccess [] cmdtysRes ++ repeat ""), (acc, amt) /= ("", "") ]
    where pairs = zipDefault "" (formSuccess [] acctsRes) (formSuccess [] amtsRes)

  -- Parse values and check for incomplete rows with only an account or an amount.
  -- The boolean in unfoldr state is for special handling of 'missingamt', where
  -- one row may have only an account and not an amount.
  postings :: [(Text, Text, Text, Either (Maybe Text, Maybe Text) Posting)]
  postings = unfoldr go (True, rows)
    where
      go (True, (x, "", c):y:xs) = Just ((x, "", c, zipRow (checkAccount x) (Left missingamount)), (True, y:xs))
      go (True, (x, "", c):xs) = Just ((x, "", c, zipRow (checkAccount x) (Right missingamt)), (False, xs))
      go (False, (x, "", c):xs) = Just ((x, "", c, zipRow (checkAccount x) (Left missingamount)), (False, xs))
      go (_, ("", y, c):xs) = Just (("", y, c, zipRow (Left missingaccount) (checkAmount c y)), (False, xs))
      go (_, (x, y, c):xs) = Just ((x, y, c, zipRow (checkAccount x) (checkAmount c y)), (True, xs))
      go (_, []) = Nothing

  zipRow (Left e) (Left e') = Left (Just e, Just e')
  zipRow (Left e) (Right _) = Left (Just e, Nothing)
  zipRow (Right _) (Left e) = Left (Nothing, Just e)
  zipRow (Right acct') (Right amt) = Right (nullposting {paccount = acct, preal = atype, pamount = mixedAmount amt'})
    where
      acct = accountNameWithoutPostingType acct'
      atype = accountNamePostingType acct'
      amt' = if isRevenue acct && amt /= missingamt then negate amt else amt

  -- The parser's own message stays in English, inside a translated frame.
  -- (The signature keeps this polymorphic under MonoLocalBinds, since it mentions trs.)
  errorToFormMsg :: Either HledgerParseErrors a -> Either Text a
  errorToFormMsg = first ((\e -> trf trs "Invalid value: {error}" [("error", e)]) . T.pack .
                          foldl (\s a -> s <> parseErrorTextPretty a) "" .
                          bundleErrors)
  checkAccount = errorToFormMsg . runParser (accountnamep <* eof) "" . T.strip
  -- the currency picked beside an amount applies unless the amount names one itself
  checkAmount c = errorToFormMsg . runParser (evalStateT (amountp <* eof) nulljournal) "" . withCommodity c . T.strip
  withCommodity c amt
    | T.null c || T.null amt || T.any (\ch -> isAlpha ch || ch `elem` ("$\8364\163\8381\165" :: String)) amt = amt
    | T.all isAlpha c = amt <> " " <> c
    | T.isPrefixOf "-" amt = "-" <> c <> T.drop 1 amt
    | otherwise = c <> amt

  -- Add errors to forms with zero rows if the form is not a FormMissing
  result :: [(Text, Text, Text, Either (Maybe Text, Maybe Text) Posting)]
  result = case (acctsRes, amtsRes) of
    -- nothing submitted: show the defaults, an existing transaction's rows
    (FormMissing, FormMissing) -> [(acc, amt, c, Right nullposting) | (acc, amt, c) <- defaults]
    _ -> case postings of
      [] -> [ ("", "", "", Left (Just missingaccount, Just missingamount))
           , ("", "", "", Left (Just missingaccount, Nothing))
           ]
      xs -> xs

  -- Prepare rows for rendering - resolve Eithers into error messages and pad to
  -- at least two rows (the form has a button for more)
  display' = flip fmap result $ \(acc, amt, c, res) -> case res of
    Left (mAccountErr, mAmountErr) -> (acc, amt, c, mAccountErr, mAmountErr)
    Right _ -> (acc, amt, c, Nothing, Nothing)
  display = display' ++ replicate (2 - length display') ("", "", "", Nothing, Nothing)

  -- And finally prepare the final FormResult [Posting]
  formResult = case traverse (\(_, _, _, x) -> x) result of
    Left _ -> FormFailure [tr trs "Postings validation failed"]
    Right xs -> FormSuccess xs

  in (formResult, zip [(1 :: Int)..] display)

zipDefault :: a -> [a] -> [a] -> [(a, a)]
zipDefault def (b:bs) (c:cs) = (b, c):(zipDefault def bs cs)
zipDefault def (b:bs) [] = (b, def):(zipDefault def bs [])
zipDefault def [] (c:cs) = (def, c):(zipDefault def [] cs)
zipDefault _ _ _ = []

formSuccess :: a -> FormResult a -> a
formSuccess def res = case res of
  FormSuccess x -> x
  _ -> def
