{-# LANGUAGE OverloadedStrings #-}

{-|
A journal entry summed up as a phone money app shows it: what it was for,
how much, and where the money came from or went to.
-}
module Hledger.Web.Widget.Entry
  ( EntryKind(..)
  , EntrySummary(..)
  , summarizeEntry
  , entryAccounts
  , entryMainAccount
  , entryMainOtherAccount
  , entryKindClass
  , dayWithWeekday
  , payeeAndNote
  ) where

import Data.List (nub, sortOn)
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import Data.Ord (Down(..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day, defaultTimeLocale, formatTime)

import Hledger

-- | What an entry did with the money.
data EntryKind
  = Spending  -- ^ paid for something: posted to an expense account
  | Earning   -- ^ took some in: posted from an income account, or a refund
  | Moving    -- ^ moved it between the entry's other accounts
  deriving (Eq, Show)

-- | An entry, summed up.
data EntrySummary = EntrySummary
  { esKind    :: EntryKind
  , esAccount :: Maybe AccountName  -- ^ what it was for: the account of its largest expense or income; none for a transfer
  , esMore    :: Int                -- ^ how many other expense (or income) accounts it posted to
  , esAmount  :: MixedAmount        -- ^ how much: negative when spent
  , esFrom    :: [AccountName]      -- ^ where the money came from: the other accounts it left
  , esTo      :: [AccountName]      -- ^ where the money went: the other accounts it reached
  } deriving (Show)

-- | Sum up an entry, by its accounts' types. With income, it is earning:
-- what came in from the income accounts. Otherwise with expenses, it is
-- spending: what went to the expense accounts. Otherwise with an expense
-- account credited, it is a refund, which is earning too. Otherwise it
-- moved money between its other accounts (assets, liabilities, equity), and
-- its amount is what reached them.
summarizeEntry :: (AccountName -> Maybe AccountType) -> Transaction -> EntrySummary
summarizeEntry atype t
  | not (null incomes)  = summary Earning incomes (maNegate $ sumPostings incomes)
  | not (null spending) = summary Spending spending (maNegate $ sumPostings spending)
  | not (null refunds)  = summary Earning refunds (maNegate $ sumPostings refunds)
  | otherwise           = EntrySummary Moving Nothing 0 (sumPostings $ filter positive others) from to
  where
    ps = filter (not . mixedAmountLooksZero . pamount) $ tpostings t
    ofType ty p = maybe False (`isAccountSubtypeOf` ty) $ atype (paccount p)
    negative p = isNegativeMixedAmount (pamount p) == Just True
    positive p = isNegativeMixedAmount (pamount p) == Just False
    incomes = filter (ofType Revenue) ps
    expenses = filter (ofType Expense) ps
    spending = filter positive expenses
    refunds = filter negative expenses
    others = filter (\p -> not (ofType Revenue p || ofType Expense p)) ps
    from = nub [paccount p | p <- others, negative p]
    to = nub [paccount p | p <- others, positive p]
    -- the account of the largest of these postings, and how many others
    summary kind qs amt =
      let accts = nub $ map paccount $ sortOn (Down . size) qs
      in EntrySummary kind (listToMaybe accts) (length accts - 1) amt from to
    size p = sum [abs (aquantity a) | a <- amountsRaw (pamount p)]

-- | The accounts an entry is about, first to last: what it was for, then
-- where the money came from (a payment), went to (income), or went to and
-- came from (a transfer).
entryAccounts :: EntrySummary -> [AccountName]
entryAccounts es = maybeToList (esAccount es) ++ case esKind es of
  Spending -> esFrom es
  Earning  -> esTo es
  Moving   -> esTo es ++ esFrom es

-- | The account an entry is mainly about: what it was for, or for a
-- transfer, where the money went. A journal row names it in its
-- data-account attribute, by which a stylesheet can give it an icon.
entryMainAccount :: EntrySummary -> AccountName
entryMainAccount = fromMaybe "" . listToMaybe . entryAccounts

-- | The account an entry is mainly about, other than those of a register
-- (matched by its account query): for a payment from a card, what it paid
-- for; for a payment to it, where the money came from. The entry's main
-- account when they are all the register's own.
entryMainOtherAccount :: Query -> EntrySummary -> AccountName
entryMainOtherAccount acctq es =
  fromMaybe (entryMainAccount es) $ listToMaybe [a | a <- entryAccounts es, not (matchesAccount acctq a)]

-- | The class naming an entry's kind, which colors its amount.
entryKindClass :: EntryKind -> Text
entryKindClass Spending = "spending"
entryKindClass Earning  = "earning"
entryKindClass Moving   = "moving"

-- | A date with its day of the week: 2026-10-21 (Wed).
dayWithWeekday :: Day -> Text
dayWithWeekday = T.pack . formatTime defaultTimeLocale "%F (%a)"

-- | An entry's payee and note, from its description, between a dot; the
-- description when it has no note.
payeeAndNote :: Transaction -> Text
payeeAndNote t = T.intercalate " · " $ filter (not . T.null) [payee, note]
  where (payee, note) = payeeAndNoteFromDescription' $ tdescription t
