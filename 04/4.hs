{-# LANGUAGE OverloadedStrings #-}

import Crypto.Hash (Digest, MD5, digestToHexByteString, hash)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (append, isPrefixOf, pack)
import System.Environment (getArgs)


main = do
  prefix <- head <$> getArgs
  print $ partOne prefix
  print $ partTwo prefix
  
hashHex :: ByteString -> ByteString
hashHex = digestToHexByteString . (hash :: ByteString -> Digest MD5)

findPrefix :: ByteString -> ByteString -> Int -> Bool
findPrefix input prefix num = isPrefixOf prefix $ hashHex (append input $ pack (show num))

partOne :: String -> Int
partOne input = head $ filter (findPrefix (pack input) "00000") [1..]

partTwo :: String -> Int
partTwo input = head $ filter (findPrefix (pack input) "000000") [1..]