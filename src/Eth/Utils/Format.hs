{-# LANGUAGE OverloadedStrings #-}

module Eth.Utils.Format where

import           Data.Char    (isAsciiUpper, isAsciiLower)
import           Data.Text    (Text)
import qualified Data.Text as T

addressLengthExceptHexPrefix :: Int
addressLengthExceptHexPrefix = 26

hexPrefix :: String
hexPrefix = "0x"

hexPrefixLength :: Int
hexPrefixLength = 2

formatTextTopicAsEthAddress :: Text -> Text
formatTextTopicAsEthAddress text = T.append (T.pack hexPrefix) (T.drop addressLengthExceptHexPrefix text)

formatTextTopicAsEthAddressStr :: Text -> String
formatTextTopicAsEthAddressStr text =  hexPrefix ++ drop addressLengthExceptHexPrefix (T.unpack text)

formatTxDataAsAmount :: Int -> Text -> String
formatTxDataAsAmount dec txData = formatAmount dec (show (hexStringToInteger (txDataWithoutHexPrefix txData)))

formatAmount :: Int -> String -> String
formatAmount dec str = (\dec'' str'' -> format dec'' str'' (length str'')) dec (appendZerosToReachDecimals dec str)
    where format dec'' str'' strLen'' = take (strLen'' - dec'') str'' ++ "." ++ drop (strLen'' - dec'') str''

appendZerosToReachDecimals :: Int -> String -> String
appendZerosToReachDecimals dec str =
    if length str > dec then str
    else appendZerosToReachDecimals dec ("0" ++ str)

txDataWithoutHexPrefix :: Text -> String
txDataWithoutHexPrefix txData = drop hexPrefixLength (T.unpack txData)

hexStringToInteger :: String -> Integer
hexStringToInteger [] = 0
hexStringToInteger str = fromIntegral z + 16 * hexStringToInteger (init str)
    where z = let y = last str in
            if isAsciiUpper y then fromEnum y - 55
            else if isAsciiLower y then fromEnum y - 87
            else fromEnum y - 48
