-- | Ed25519 signatures over certificate hashes.
--
-- A signature covers the certificate's hash, so a single certificate can
-- be verified by a party that has neither the ledger nor the policy.
-- Post-quantum dual signing (ML-DSA) is on the roadmap.
module Palisade.Sign
  ( SigningKey
  , generateKey
  , encodeSecret
  , decodeSecret
  , publicKeyHex
  , signHash
  , verifySignature
  , toHex
  , fromHex
  ) where

import Crypto.Error (CryptoFailable (..))
import qualified Crypto.PubKey.Ed25519 as Ed
import Data.Aeson (Value, object, withObject, (.:), (.=))
import Data.Aeson.Types (parseEither)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.Char (digitToInt, intToDigit, isHexDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)

data SigningKey = SigningKey Ed.SecretKey Ed.PublicKey

generateKey :: IO SigningKey
generateKey = do
  sk <- Ed.generateSecretKey
  pure (SigningKey sk (Ed.toPublic sk))

encodeSecret :: SigningKey -> Text
encodeSecret (SigningKey sk _) = toHex (BA.convert sk)

decodeSecret :: Text -> Either Text SigningKey
decodeSecret t = do
  bs <- fromHex (T.strip t)
  case Ed.secretKey bs of
    CryptoPassed sk -> Right (SigningKey sk (Ed.toPublic sk))
    CryptoFailed e -> Left ("not an Ed25519 secret key: " <> T.pack (show e))

publicKeyHex :: SigningKey -> Text
publicKeyHex (SigningKey _ pk) = toHex (BA.convert pk)

-- | Sign a hex hash; returns the JSON stored next to the entry.
signHash :: SigningKey -> Text -> Value
signHash (SigningKey sk pk) h =
  object
    [ "alg" .= ("ed25519" :: Text)
    , "public_key" .= toHex (BA.convert pk)
    , "signature" .= toHex (BA.convert (Ed.sign sk pk (TE.encodeUtf8 h)))
    ]

-- | Verify a signature value over a hash. Returns the signer's public key.
verifySignature :: Value -> Text -> Either Text Text
verifySignature v h = do
  (alg, pkHex, sigHex) <- either (Left . T.pack) Right $
    flip parseEither v $ withObject "signature" $ \o ->
      (,,) <$> o .: "alg" <*> o .: "public_key" <*> o .: "signature"
  if alg /= ("ed25519" :: Text) then Left ("unsupported signature algorithm " <> alg) else Right ()
  pkBytes <- fromHex pkHex
  sigBytes <- fromHex sigHex
  pk <- case Ed.publicKey pkBytes of
    CryptoPassed k -> Right k
    CryptoFailed e -> Left ("bad public key: " <> T.pack (show e))
  sig <- case Ed.signature sigBytes of
    CryptoPassed s -> Right s
    CryptoFailed e -> Left ("bad signature encoding: " <> T.pack (show e))
  if Ed.verify pk (TE.encodeUtf8 h) sig then Right pkHex else Left "signature does not verify"

toHex :: BS.ByteString -> Text
toHex = T.pack . concatMap byte . BS.unpack
  where
    byte w = [intToDigit (fromIntegral (w `shiftR` 4)), intToDigit (fromIntegral (w .&. 15))]

fromHex :: Text -> Either Text BS.ByteString
fromHex t
  | odd (T.length t) = Left "hex string has odd length"
  | not (T.all isHexDigit t) = Left "not a hex string"
  | otherwise = Right (BS.pack (pairs (T.unpack t)))
  where
    pairs :: String -> [Word8]
    pairs (a : b : rest) = (fromIntegral (digitToInt a) `shiftL` 4 .|. fromIntegral (digitToInt b)) : pairs rest
    pairs _ = []

