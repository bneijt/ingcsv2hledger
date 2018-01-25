{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( transformIngFilesToHLedger
    ) where

import Data.Csv.Parser (csvWithHeader, defaultDecodeOptions)
import Data.Csv (NamedRecord)
import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString as BS
import Data.HashMap.Strict ((!))

import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text as T
import Data.Text (Text(..))
import Data.Vector (toList)
import Data.Monoid ((<>))
import qualified Data.Text.IO as TIO
import Data.Text.Read (double)
import Data.List (nubBy, sortBy, nub)
import Data.Ord (comparing)

import TextShow (showt, toText)
import TextShow.Data.Floating (showbFFloat)
import Data.Decimal
import Data.Maybe

import Ing (loadIngCsvFile)
import qualified Model as M
import  Data.Time (formatTime, defaultTimeLocale, zonedTimeToLocalTime)

showAmount :: Decimal -> Text
showAmount d = T.pack $ show d

formattedDate :: M.Transaction -> Text
formattedDate t = T.pack $ formatTime defaultTimeLocale "%F" (M.transactionTime t)


hLedgerRecordFrom :: M.Transaction -> Text
hLedgerRecordFrom transaction =
    ""
    <> (formattedDate transaction) <> " " <> (M.humanizedTransactionContext $ M.transactionContext transaction) <> ": " <> (M.description transaction) <> "\n"
    <> (if T.length (M.accountIBAN (M.decAccount transaction)) > 0
        then "    accounts:iban:" <> M.accountIBAN (M.decAccount transaction) <> "   €" <> (showAmount $ -(M.amount transaction)) <> "\n"
        else "")
    <> (if T.length (M.accountIBAN (M.incAccount transaction)) > 0
        then "    accounts:iban:" <> M.accountIBAN (M.incAccount transaction) <> "  €" <> (showAmount $ M.amount transaction)  <> "\n"
        else "    assets:cash\n")


loadtransactionsFrom :: FilePath -> IO [M.Transaction]
loadtransactionsFrom filepath = do
    eitherTransactionOrFailure <- loadIngCsvFile filepath
    case eitherTransactionOrFailure of
        Right transactions -> return transactions
        Left problem -> error (T.unpack problem)


transformIngFilesToHLedger :: [FilePath] -> IO ()
transformIngFilesToHLedger files = do
    transactionsPerFile <- mapM loadtransactionsFrom files
    let allTransactions = concat transactionsPerFile
    let sortedTransactions = sortBy (comparing $ zonedTimeToLocalTime . M.transactionTime) allTransactions
    let uniqueTransactions = nub sortedTransactions
    mapM_ (\t -> TIO.putStrLn (hLedgerRecordFrom t)) uniqueTransactions
