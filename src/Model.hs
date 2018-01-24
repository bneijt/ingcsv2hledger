{-# LANGUAGE OverloadedStrings #-}
module Model
    (
        AccountHolder(..),
        emptyAccountHolder,
        Account(..),
        emptyAccount,
        TransactionContext(..),
        emptyTransactionContext,
        humanizedTransactionContext,
        Transaction(..),
        emptyTransaction
    ) where

import Data.UUID (UUID(..))
import Data.Time (ZonedTime)
import Data.Text (Text(..))
import Data.Decimal (Decimal(..))
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))

data AccountHolder = AccountHolder {
    name :: Text
} deriving (Show)

emptyAccountHolder = AccountHolder {
    name = ""
}

data Account = Account {
    accountIBAN :: Text,
    accountHolder :: AccountHolder
} deriving (Show)

emptyAccount = Account {
    accountIBAN = "",
    accountHolder = emptyAccountHolder
}

data TransactionContext = PayTerminal {
    cardUsed :: Text,
    paymentTime :: ZonedTime,
    terminalCode :: Text,
    transactionIdentifier :: Text
    }
    | Withdrawl
    | Periodic
    | Reserved
    | Rent
    | Web
    | Recessed -- Incasso in dutch
    | Other
  deriving (Show)

humanizedTransactionContext :: TransactionContext -> Text
humanizedTransactionContext trxc = case trxc of
    PayTerminal card paymentTime terminalCode transactionIdentifier -> "Payment at terminal with card " <> card
    Withdrawl -> "Withdrawl"
    Periodic  -> "Periodic"
    Reserved  -> "Reserved"
    Rent      -> "Rent"
    Web       -> "Web"
    Recessed  -> "Recessed"
    Other     -> "Other"

emptyTransactionContext = Other

data Transaction = Transaction {
    transactionTime :: ZonedTime,
    decAccount :: Account, -- Decrementing account
    incAccount :: Account, -- Incrementing account
    amount :: Decimal, -- Amount
    currency :: Text,
    comment :: Text,
    description :: Text,
    transactionContext :: TransactionContext
} deriving (Show)


emptyTransaction :: Transaction
emptyTransaction = Transaction {
    transactionTime = fromJust $ parseTimeM True defaultTimeLocale "%Y-%-m-%-d" "2010-3-04",
    decAccount = emptyAccount,
    incAccount = emptyAccount,
    amount = 0,
    currency = "",
    comment = "",
    description = "",
    transactionContext = emptyTransactionContext
}
