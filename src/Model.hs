{-# LANGUAGE OverloadedStrings #-}
module Model
    (
        AccountHolder(..),
        emptyAccountHolder,
        Account(..),
        emptyAccount,
        TransactionContext(..),
        emptyTransactionContext,
        Transaction(..),
        emptyTransaction
    ) where

import Data.UUID (UUID(..))
import Data.Time (ZonedTime)
import Data.Text (Text(..))
import Data.Decimal (Decimal(..))
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Maybe (fromJust)

data AccountHolder = AccountHolder {
    name :: Text
}

emptyAccountHolder = AccountHolder {
    name = ""
}

data Account = Account {
    accountIBAN :: Text,
    accountHolder :: AccountHolder
}

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
    | Periodic
    | Reserved
    | Rent
    | Internet
    | Recessed -- Incasso in dutch
    | Other

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
}


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
