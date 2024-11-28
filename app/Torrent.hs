module Torrent
  ( -- generateURLEncodedInfoHash,
    -- calculateInfoHash,
    -- calculateHash,
    -- calculateHexHash,
    getPieceHashes,
    makeQuery,
    peersToAddressList,
    addressToIPAndPort,
    parsePort,
    TorrentType (..),
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (ord)
import Data.Map (Map)
import Data.Map.Internal ((!))
import Decoder (DecodedValue, decodedToByteString, decodedToDictionary, generateURLEncodedInfoHash, sortInfo, toBencodedByteString)
import Prelude

data TorrentType = TorrentType
  { outputpath :: ByteString,
    peers :: [(ByteString, ByteString)],
    infoHash :: ByteString,
    pieceHashes :: [ByteString],
    fileLength :: Int,
    pieceLength :: Int
  }
  deriving (Show)

-- splitBy2 :: ByteString -> [ByteString]
-- splitBy2 x = if B.length x >= 2 then two : splitBy2 rest else []
--   where
--     (two, rest) = B.splitAt 2 x

-- generateURLEncodedInfoHash :: DecodedValue -> ByteString
-- generateURLEncodedInfoHash info = do
--   let urlencoded = Prelude.map (B.pack "%" <>) $ splitBy2 $ calculateInfoHash info "hex"
--   B.concat urlencoded

-- calculateInfoHash :: DecodedValue -> Prelude.String -> ByteString
-- calculateInfoHash info "hex" = calculateHexHash str
--   where
--     str = toBencodedByteString info
-- calculateInfoHash info _ = calculateHash str
--   where
--     str = toBencodedByteString info

getPieceHashes :: ByteString -> [ByteString]
getPieceHashes pieces = if B.length pieces < 20 then [] else piece : getPieceHashes rest
  where
    (piece, rest) = B.splitAt 20 pieces

makeQuery :: Map ByteString DecodedValue -> String
makeQuery json = do
  let info = decodedToDictionary (json ! B.pack "info")
  let announce = decodedToByteString $ json ! B.pack "announce"
  let len = B.pack $ show $ info ! B.pack "length"
  let infoHash = generateURLEncodedInfoHash $ sortInfo info
  B.unpack $ B.concat [announce, B.pack "?info_hash=", infoHash, B.pack "&peer_id=12349679991234567890&port=6881&uploaded=0&downloaded=0&left=", len, B.pack "&compact=1"]

peersToAddressList :: ByteString -> [(ByteString, ByteString)]
peersToAddressList input = if B.length input < 6 then [] else addressToIPAndPort ip : peersToAddressList rest
  where
    (ip, rest) = B.splitAt 6 input

addressToIPAndPort :: ByteString -> (ByteString, ByteString)
addressToIPAndPort address = do
  (B.intercalate (B.pack ".") $ map (B.pack . show . ord) (B.unpack ip), parsePort port)
  where
    (ip, port) = B.splitAt 4 address

parsePort :: ByteString -> ByteString
parsePort port = B.pack $ show $ ord (B.head port) * 256 + ord (B.last port)
