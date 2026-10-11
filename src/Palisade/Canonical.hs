-- | Canonical JSON and hashing.
--
-- Hash chains and certificates must be byte-for-byte reproducible on any
-- machine, so they are computed over a canonical encoding: object keys
-- sorted, no insignificant whitespace, and money carried as decimal
-- strings rather than JSON numbers.
module Palisade.Canonical
  ( canonical
  , canonicalText
  , sha256Hex
  , sha256HexText
  , hashValue
  , genesisHash
  ) where

import Crypto.Hash (SHA256 (..), hashWith)
import Data.Aeson (Value (..), encode)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder, lazyByteString, toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.List (intersperse, sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

canonical :: Value -> BL.ByteString
canonical = toLazyByteString . go
  where
    go :: Value -> Builder
    go = \case
      Object o ->
        let kvs = sortOn fst [(Key.toText k, v) | (k, v) <- KM.toList o]
         in "{" <> mconcat (intersperse "," [lazyByteString (encode (String k)) <> ":" <> go v | (k, v) <- kvs]) <> "}"
      Array a -> "[" <> mconcat (intersperse "," (map go (toList a))) <> "]"
      other -> lazyByteString (encode other)

canonicalText :: Value -> Text
canonicalText = TE.decodeUtf8 . BL.toStrict . canonical

-- | Lower-case hex SHA-256.
sha256Hex :: BS.ByteString -> Text
sha256Hex bs = T.pack (show (hashWith SHA256 bs))

sha256HexText :: Text -> Text
sha256HexText = sha256Hex . TE.encodeUtf8

hashValue :: Value -> Text
hashValue = sha256Hex . BL.toStrict . canonical

-- | The @prev@ of the first entry in every chain.
genesisHash :: Text
genesisHash = T.replicate 64 "0"
