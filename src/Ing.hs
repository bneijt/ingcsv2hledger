{-# LANGUAGE OverloadedStrings #-}
module Ing
(
    loadIngCsvFile
) where

import qualified Model as M
import Data.Text (Text(..), unpack, splitOn)
import qualified Data.Text as T
import IngCsv(readIngCsvFile, IngTransaction(..))

import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Maybe (fromJust)

payTerminalFrom :: IngTransaction -> M.TransactionContext
payTerminalFrom t = M.PayTerminal {
    M.cardUsed = card,
    M.paymentTime = fromJust $ parseTimeM True defaultTimeLocale "%d-%m-%Y %H:%M" (unpack transactionTimeString),
    M.terminalCode = last splitComment,
    M.transactionIdentifier = (head . T.words . head . (drop 1) . reverse) splitComment
    }
    where
        splitComment = splitOn ":" (comment t)
        card = head $ T.words $ splitComment !! 1
        transactionTimeString = T.unwords $ take 2 $ drop 1 $ T.words (comment t)


transactionContext :: IngTransaction -> M.TransactionContext
transactionContext t = case mutationTypeCode t of
    "AC" -> M.Other -- Acceptgiro
    "IC" -> M.Recessed -- Incasso
    "BA" -> payTerminalFrom t -- Betaalautomaat
    "OV" -> M.Other -- Overschrijving
    "CH" -> M.Withdrawl -- Cheque
    "PK" -> M.Other -- Opname kantoor
    "DV" -> M.Other -- Diversen
    "PO" -> M.Periodic -- Periodieke Overschrijving
    "GB" -> M.Other -- Overschrijvingskaart
    "R"  -> M.Rent -- Rente
    "GF" -> M.Other -- Telefonisch bankieren
    "RV" -> M.Reserved -- Reservering
    "GM" -> M.Withdrawl -- Geldautomaat
    "GT" -> M.Web -- Internet bankieren
    "VZ" -> M.Other -- Verzamelbetaling
    _    -> M.Other

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
    M.transactionContext = transactionContext i
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
