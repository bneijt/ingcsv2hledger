{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( transformIngFileToHLedger
    ) where

import Data.Csv.Parser (csvWithHeader, defaultDecodeOptions)
import Data.Csv (NamedRecord)
import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString as BS
import Data.HashMap.Strict ((!))

import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T
import Data.Text (Text(..))
import Data.Vector (toList)
import Data.Monoid ((<>))
import qualified Data.Text.IO as TIO
import Data.Text.Read (double)

import TextShow (showt)

-- ["Datum","Naam / Omschrijving","Rekening","Tegenrekening","Code","Af Bij","Bedrag (EUR)","MutatieSoort","Mededelingen"]
data Transaction = Transaction {
    dateOfTransaction :: Text,
    description :: Text,
    comment :: Text,
    fromAccount :: Text,
    toAccount :: Text,
    amountInEuro :: Double
} deriving (Show)


slice :: Int -> Int -> Text -> Text
slice from to value = T.take (to - from) part
    where part = T.drop from value

-- 2008/10/01 take a loan
--     assets:bank:checking  $1
--     liabilities:debts    $-1
ledgerRecordFrom :: Transaction -> Text
ledgerRecordFrom transaction =
    (slice 0 4 td) <> "/" <> (slice 4 6 td) <> "/" <> (slice 6 8 td) <> " " <> (description transaction) <> "\n"
    <> "    accounts:bank:" <> (fromAccount transaction) <> "   €" <> (showt $ amountInEuro transaction) <> "\n"
    <> (if T.length (toAccount transaction) > 0 then "    accounts:bank:" <> (toAccount transaction) <> "  €" <> (showt $ -(amountInEuro transaction))  <> "\n" else "")
    <> "\n"
    where
        td = (dateOfTransaction transaction)

doubleValue :: NamedRecord -> BS.ByteString -> Double
doubleValue r k = readDouble (textValue r k)

textValue :: NamedRecord -> BS.ByteString -> Text
textValue r k = decodeUtf8 $ r ! k

readDouble :: Text -> Double
readDouble v =
    case double v of
        Right (d, t) -> if T.length t > 0 then error("Still left with text after converting " ++ T.unpack v) else d
        Left e -> error(e)


signedDutchEuroFrom :: Text -> Text -> Double
signedDutchEuroFrom afBij amount = if positive then amountDouble else -amountDouble
    where
        amountDouble = readDouble (T.replace "," "." amount)
        positive = afBij == "Bij"

transactionFromIngRow :: NamedRecord -> Transaction
transactionFromIngRow row = Transaction {
    dateOfTransaction = (textValue row "Datum"),
    description = (textValue row "Naam / Omschrijving"),
    comment = (textValue row "Mededelingen"),
    fromAccount = (textValue row "Rekening"),
    toAccount = (textValue row "Tegenrekening"),
    amountInEuro = signedDutchEuroFrom (textValue row "Af Bij")(textValue row "Bedrag (EUR)")
}


transformIngFileToHLedger :: FilePath -> IO ()
transformIngFileToHLedger filepath = do
    csvContents <- BS.readFile filepath
    case parseOnly (csvWithHeader defaultDecodeOptions) csvContents of
        Left _ -> print "Failed to parse"
        Right (header, rows) -> do
            let transactions = map transactionFromIngRow (toList rows)
            mapM_ (\x -> TIO.putStrLn (ledgerRecordFrom x)) transactions
