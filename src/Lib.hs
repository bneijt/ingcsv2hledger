{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Data.Csv.Parser (csvWithHeader, defaultDecodeOptions)
import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Data.Csv (NamedRecord)
-- import qualified Data.Text as T
import qualified Data.Text.Lazy as L

import Data.Text.IO as TIO
-- import Data.Text.Template (substitute)
import Text.EDE (render, parse, Result(..))
import Data.HashMap.Lazy (empty, HashMap(..), foldrWithKey, insert)
-- import qualified Data.Text.Lazy as L
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T
import Data.Aeson.Types (Value(..))

-- 2008/10/01 take a loan
--     assets:bank:checking  $1
--     liabilities:debts    $-1
ledgerTemplateBody :: BS.ByteString
ledgerTemplateBody = pack "{{ Datum }}/10/01 {{ Mededelingen }}\n\
        \    assets:bank:{{ Rekening }}  €{{ amount }}\n"

ledgerTemplate = case parse ledgerTemplateBody of
    Success template -> template
    Failure f -> error(show f)

filterMapping :: T.Text -> T.Text -> (T.Text, T.Text)
filterMapping k v = if k == "Bedrag (EUR)"
    then ("amount", T.replace "," "." v)
    else (k, v)

updateWithText :: BS.ByteString -> BS.ByteString -> HashMap T.Text Value -> HashMap T.Text Value
updateWithText k v map = insert k' (String v') map
    where
        (k', v') = filterMapping (decodeUtf8 k) (decodeUtf8 v)

contextOf record = foldrWithKey updateWithText (empty :: HashMap T.Text Value) record


printLedgerRecordFor :: NamedRecord -> IO()
printLedgerRecordFor record = case render ledgerTemplate (contextOf record) of
    Success value -> TIO.putStrLn (L.toStrict value)
    Failure _ -> print ("FAIL")


someFunc :: IO ()
someFunc = do
    csvContents <- BS.readFile "ing.csv"
    case parseOnly (csvWithHeader defaultDecodeOptions) csvContents of
        Left _ -> print "Failed to parse"
        Right (header, rows) -> do
            mapM_ printLedgerRecordFor rows
            print header
