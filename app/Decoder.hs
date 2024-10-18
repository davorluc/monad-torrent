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
  )
where

import Data.ByteString.Char8 (ByteString, unpack)
import qualified Data.ByteString.Char8 as B
import Data.Char (ord)
import Data.Functor (void)
import Data.List (intercalate, sortBy)
import Data.Map (Map, assocs, fromList, toList)
import Data.Void (Void)
import Data.Word (Word8)
import Text.Megaparsec
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

type Parser = Parsec Void ByteString

integer :: Parser Int
integer = L.decimal

pChar :: Char -> Parser Word8
pChar c = char (fromIntegral $ ord c)

noSpace :: Parser ()
noSpace = L.space empty empty empty

signedInteger :: Parser Int
signedInteger = L.signed noSpace integer

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
decodedToByteString _ = error "Invalid decoded value in decodedToByteString"

decodedToDictionary :: DecodedValue -> Map ByteString DecodedValue
decodedToDictionary (Dict dictionary) = dictionary
decodedToDictionary _ = error "Invalid decoded value in decodedToDictionary"

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