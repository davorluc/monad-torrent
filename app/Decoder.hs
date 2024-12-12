{-# LANGUAGE OverloadedStrings #-}

module Decoder
  ( DecodedValue (..),
    Parser,
    integer,
    pChar,
    noSpace,
    signedInteger,
    pBencodedInt,
    pBencodedByteString,
    pBencodedList,
    pBencodedDict,
    decodedToByteString,
    decodedToDictionary,
    decodedToList,
    pBencodedKeyValuePair,
    pBencodedValue,
    decodeBencodedValue,
    sortInfo,
    toBencodedByteString,
    getPieces,
    intToHexByteString,
    padWithZeros,
    isLetterByte,
    makeBigEndian,
    makeQuery,
    calculateInfoHash,
    calculateHash,
    calculateHexHash,
    generateURLEncodedInfoHash,
  )
where

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Builder (int32BE, toLazyByteString)
import Data.ByteString.Char8 (ByteString, unpack)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Char (chr, isLetter, ord)
import Data.Functor (void)
import Data.Int (Int32)
import Data.List (intercalate, sortBy)
import Data.Map (Map, assocs, fromList, toList, (!))
import Data.Void (Void)
import Data.Word (Word8)
import Text.Megaparsec
  ( MonadParsec (takeP),
    Parsec,
    empty,
    many,
    parseMaybe,
    (<?>),
    (<|>),
  )
import Text.Megaparsec.Byte (char)
import qualified Text.Megaparsec.Byte.Lexer as L

data DecodedValue = Int Int | ByteString ByteString | List [DecodedValue] | Dict (Map ByteString DecodedValue) deriving (Eq, Ord)

instance Show DecodedValue where
  show (Int i) = show i
  show (ByteString s) = "\"" <> unpack s <> "\""
  show (List l) = show l
  show (Dict d) = "{" ++ intercalate "," pairs ++ "}"
    where
      pairs = map (\(k, v) -> concat [show k, ":", show v]) (assocs d)

type Parser = Parsec Void B.ByteString

integer :: Parser Int
integer = L.decimal

pChar :: Char -> Parser Word8
pChar c = char (fromIntegral $ ord c)

noSpace :: Parser ()
noSpace = L.space empty empty empty

signedInteger :: Parser Int
signedInteger = L.signed noSpace integer

isLetterByte :: Word8 -> Bool
isLetterByte = isLetter . chr . fromIntegral

-- i42e
pBencodedInt :: Parser DecodedValue
pBencodedInt = do
  void (pChar 'i')
  int <- Int <$> signedInteger
  void (pChar 'e')
  return int

-- 4:spam
pBencodedByteString :: Parser DecodedValue
pBencodedByteString = do
  length <- L.decimal
  void (pChar ':')
  ByteString <$> takeP (Just "String") length

-- l4:spam4:eggsi42ee
pBencodedList :: Parser DecodedValue
pBencodedList = do
  void (pChar 'l')
  bencodedList <- List <$> many pBencodedValue <?> "list values"
  void (pChar 'e')
  return bencodedList

-- {key: value, key: value}
pBencodedDict :: Parser DecodedValue
pBencodedDict = do
  void (pChar 'd')
  dictionary <- Dict . fromList <$> many pBencodedKeyValuePair
  void (pChar 'e')
  return dictionary

decodedToByteString :: DecodedValue -> ByteString
decodedToByteString (ByteString str) = str
decodedToByteString value = error $ "Invalid decoded value in decodedToByteString: " <> show value

decodedToDictionary :: DecodedValue -> Map B.ByteString DecodedValue
decodedToDictionary (Dict dictionary) = dictionary

decodedToList :: DecodedValue -> [DecodedValue]
decodedToList (List list) = list
decodedToList _ = error "Invalid decoded value in decodedToList"

pBencodedKeyValuePair :: Parser (ByteString, DecodedValue)
pBencodedKeyValuePair = (,) <$> (decodedToByteString <$> pBencodedByteString) <*> pBencodedValue

pBencodedValue :: Parser DecodedValue
pBencodedValue = pBencodedList <|> pBencodedInt <|> pBencodedByteString <|> pBencodedDict

decodeBencodedValue :: ByteString -> DecodedValue
decodeBencodedValue encodedValue =
  case parseMaybe pBencodedValue encodedValue of
    Just value -> value
    Nothing -> error $ "Unhandled encoded value: " <> show encodedValue

sortInfo :: Map ByteString DecodedValue -> DecodedValue
sortInfo m = Dict (fromList $ sortBy (\(a1, _) (b1, _) -> if a1 > b1 then GT else LT) $ toList m)

toBencodedByteString :: DecodedValue -> ByteString
toBencodedByteString (Int num) = B.singleton 'i' <> B.pack (show num) <> B.singleton 'e'
toBencodedByteString (ByteString bytestring) = B.pack (show (B.length bytestring)) <> B.singleton ':' <> bytestring
toBencodedByteString (List list) = B.singleton 'l' <> B.concat (map toBencodedByteString list) <> B.singleton 'e'
toBencodedByteString (Dict dictionary) = B.singleton 'd' <> B.concat pairs <> B.singleton 'e'
  where
    pairKeyValue :: (ByteString, DecodedValue) -> ByteString
    pairKeyValue (key, value) = toBencodedByteString (ByteString key) <> toBencodedByteString value
    pairList = assocs dictionary
    pairs = map pairKeyValue pairList

getPieces :: ByteString -> [ByteString]
getPieces pieces = if B.length pieces < 20 then [] else piece : getPieces rest
  where
    (piece, rest) = B.splitAt 20 pieces

splitBy2 :: ByteString -> [ByteString]
splitBy2 x = if B.length x >= 2 then two : splitBy2 rest else []
  where
    (two, rest) = B.splitAt 2 x

generateURLEncodedInfoHash :: DecodedValue -> ByteString
generateURLEncodedInfoHash info = do
  let urlencoded = map ("%" <>) $ splitBy2 $ calculateInfoHash info "hex"
  B.concat urlencoded

calculateInfoHash :: DecodedValue -> String -> ByteString
calculateInfoHash info "hex" = calculateHexHash str
  where
    str = toBencodedByteString info
calculateInfoHash info _ = calculateHash str
  where
    str = toBencodedByteString info

calculateHash :: ByteString -> ByteString
calculateHash = SHA1.hash

calculateHexHash :: ByteString -> ByteString
calculateHexHash = B16.encode . SHA1.hash

makeQuery :: Map B.ByteString DecodedValue -> String
makeQuery json = do
  let info = decodedToDictionary (json ! "info")
  let announce = decodedToByteString $ json ! "announce"
  let length = B.pack $ show $ info ! "length"
  let infoHash = generateURLEncodedInfoHash $ sortInfo info
  B.unpack $ B.concat [announce, "?info_hash=", infoHash, "&peer_id=12349679991234567890&port=6881&uploaded=0&downloaded=0&left=", length, "&compact=1"]

-- peersToAddressList :: ByteString -> [(ByteString, ByteString)]
-- peersToAddressList input = if B.length input < 6 then [] else addressToIPAndPort ip : peersToAddressList rest
--   where
--     (ip, rest) = B.splitAt 6 input

-- addressToIPAndPort :: ByteString -> (ByteString, ByteString)
-- addressToIPAndPort address = do
--   (B.intercalate "." $ map (B.pack . show . ord) (B.unpack ip), parsePort port)
--   where
--     (ip, port) = B.splitAt 4 address

-- parsePort :: ByteString -> ByteString
-- parsePort port = B.pack $ show $ ord (B.head port) * 256 + ord (B.last port)

intToHexByteString :: Int32 -> ByteString
intToHexByteString n = B.toStrict $ toLazyByteString $ int32BE n

padWithZeros :: ByteString -> ByteString
padWithZeros bs
  | B.length bs >= 4 = bs -- If length is already >= 4, return the ByteString as is
  | otherwise = B.replicate (4 - B.length bs) (B.head $ B.toStrict $ LB.singleton (0 :: Word8)) `B.append` bs

makeBigEndian :: ByteString -> ByteString
makeBigEndian bs = B.concat [last, first]
  where
    (first, last) = B.splitAt 2 bs
