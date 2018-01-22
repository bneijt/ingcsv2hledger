{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( transformIngFileToHLedger
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

import TextShow (showt, toText)
import TextShow.Data.Floating (showbFFloat)
import Data.Decimal
import Data.Maybe

import Ing (loadIngCsvFile)
import qualified Model as M

showAmount :: Decimal -> Text
showAmount d = T.pack $ show d


hLedgerRecordFrom :: M.Transaction -> Text
hLedgerRecordFrom transaction =
    ""
    <> "2012-01-01" <> " " <> "description" <> "\n"
    <> "    accounts:bank:" <> M.accountIBAN (M.decAccount transaction) <> "   €" <> (showAmount $ -(M.amount transaction)) <> "\n"
    <> (if T.length (M.accountIBAN (M.incAccount transaction)) > 0
        then "    accounts:bank:" <> M.accountIBAN (M.incAccount transaction) <> "  €" <> (showAmount $ M.amount transaction)  <> "\n"
        else "    assets:cash\n")
    -- <> "\n"
    -- where
    --     td = (dateOfTransaction transaction)



transformIngFileToHLedger :: FilePath -> IO ()
transformIngFileToHLedger filepath = do
    eitherTransactionOrFailure <- loadIngCsvFile filepath
    case eitherTransactionOrFailure of
        Right transactions -> mapM_ (\t -> TIO.putStrLn (hLedgerRecordFrom t)) transactions
        Left problem -> print problem
