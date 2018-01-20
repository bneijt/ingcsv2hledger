{-# LANGUAGE OverloadedStrings #-}
module IngCsv
    (
        readIngCsvFile,
        IngTransaction(..)
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
import Data.Either



-- ["Datum","Naam / Omschrijving","Rekening","Tegenrekening","Code","Af Bij","Bedrag (EUR)","MutatieSoort","Mededelingen"]
-- AC = Acceptgiro, IC = Incasso, BA = Betaalautomaat,
-- OV = Overschrijving, CH = Cheque, PK = Opname kantoor,
-- DV = Diversen, PO = Periodieke Overschrijving, GB = Overschrijvingskaart,
-- R = Rente, GF = Telefonisch bankieren, RV = Reservering,
-- GM = Geldautomaat, BA = Betaalautomaat, GT = Internet bankieren,
-- VZ = Verzamelbetaling.
data IngTransaction = IngTransaction {
    dateOfTransaction :: Text,
    description :: Text,
    fromAccount :: Text,
    toAccount :: Text,
    code :: Text,
    afBij :: Text,
    amountInEuro :: Decimal,
    mutationType :: Text,
    comment :: Text
} deriving (Show)


slice :: Int -> Int -> Text -> Text
slice from to value = T.take (to - from) part
    where part = T.drop from value

showAmount :: Decimal -> Text
showAmount d = T.pack $ show d


decimalValue :: NamedRecord -> BS.ByteString -> Decimal
decimalValue r k = readDecimal $ textValue r k

textValue :: NamedRecord -> BS.ByteString -> Text
textValue r k = decodeUtf8 $ r ! k

readDecimal :: Text -> Decimal
readDecimal v = read (T.unpack v)

-- ["Datum","Naam / Omschrijving","Rekening","Tegenrekening","Code","Af Bij","Bedrag (EUR)","MutatieSoort","Mededelingen"]
transactionFromIngRow :: NamedRecord -> IngTransaction
transactionFromIngRow row = IngTransaction {
    dateOfTransaction = (textValue row "Datum"),
    description = (textValue row "Naam / Omschrijving"),
    fromAccount = (textValue row "Rekening"),
    toAccount = (textValue row "Tegenrekening"),
    code = (textValue row "Code"),
    afBij = (textValue row "Af Bij"),
    amountInEuro = readDecimal (T.replace "," "." (textValue row "Bedrag (EUR)")),
    mutationType = (textValue row "MutatieSoort"),
    comment = (textValue row "Mededelingen")
}



readIngCsvFile :: FilePath -> IO (Either Text [IngTransaction])
readIngCsvFile filepath = do
    csvContents <- BS.readFile filepath
    case parseOnly (csvWithHeader defaultDecodeOptions) csvContents of
        Left _ -> return $ Left "Failed to parse"
        Right (header, rows) -> do
            return $ Right $ map transactionFromIngRow (toList rows)
