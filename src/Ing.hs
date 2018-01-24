{-# LANGUAGE OverloadedStrings #-}
module Ing
(
    loadIngCsvFile
) where

import qualified Model as M
import Data.Text (Text(..), unpack)
import IngCsv(readIngCsvFile, IngTransaction(..))

import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Maybe (fromJust)


toTransaction :: IngTransaction -> M.Transaction
toTransaction i = M.Transaction {
    M.transactionTime = fromJust $ parseTimeM True defaultTimeLocale "%Y%m%d" (unpack $ dateOfTransaction i),
    M.decAccount = M.Account {
        M.accountIBAN = decrementingAccount,
        M.accountHolder = M.emptyAccountHolder
    },
    M.incAccount = M.Account {
        M.accountIBAN = incrementingAccount,
        M.accountHolder = M.emptyAccountHolder
    },
    M.amount = amountInEuro i,
    M.currency = "€",
    M.comment = comment i,
    M.description = description i,
    M.transactionContext = M.Other
    }
    where
        decrementingAccount = if afBij i == "Af" then (fromAccount i) else (toAccount i)
        incrementingAccount = if afBij i == "Af" then (toAccount i) else (fromAccount i)

loadIngCsvFile :: FilePath -> IO (Either Text [M.Transaction])
loadIngCsvFile filepath = do
    ingRecords <- readIngCsvFile filepath
    case ingRecords of
        Left failureInformation -> return $ Left $ failureInformation
        Right rows -> do
            return $ Right $ map toTransaction rows
