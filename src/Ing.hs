{-# LANGUAGE OverloadedStrings #-}
module Ing
(
    loadIngCsvFile
) where

import qualified Model as M
import Data.Text (Text(..))
import IngCsv(readIngCsvFile, IngTransaction(..))

import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Maybe (fromJust)


toTransaction :: IngTransaction -> M.Transaction
toTransaction i = M.Transaction {
    M.transactionTime = fromJust $ parseTimeM True defaultTimeLocale "%Y-%-m-%-d" "2010-3-04",
    M.decAccount = M.Account {
        M.accountIBAN = "",
        M.accountHolder = M.emptyAccountHolder
    },
    M.incAccount = M.Account {
        M.accountIBAN = "",
        M.accountHolder = M.emptyAccountHolder
    },
    M.amount = 0,
    M.currency = "€",
    M.comment = "",
    M.description = "",
    M.transactionContext = M.Other
}

loadIngCsvFile :: FilePath -> IO (Either Text [M.Transaction])
loadIngCsvFile filepath = do
    ingRecords <- readIngCsvFile filepath
    case ingRecords of
        Left failureInformation -> return $ Left $ failureInformation
        Right rows -> do
            return $ Right $ map toTransaction rows
