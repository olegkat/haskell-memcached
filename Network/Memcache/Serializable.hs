-- Memcached interface.
-- Copyright (C) 2005 Evan Martin <martine@danga.com>

module Network.Memcache.Serializable(Serializable, serialize, deserialize) where

import Prelude hiding (head)
import Data.ByteString (ByteString)
import Data.Text hiding (map)
import Data.Text.Encoding

-- It'd be nice to use "show" for serialization, but when we
-- serialize a String we want to serialize it without the quotes.

-- TODO:
--  - allow serializing bytes as Ptr
--    to do this, add a "putToHandle", etc. method in Serializable
--    where the default uses serialize, but for Ptr uses socket stuff.

--import Foreign.Marshal.Utils
--import Foreign.Storable (Storable, sizeOf)

class Serializable a where
  serialize    :: a -> ByteString
  deserialize  :: ByteString -> Maybe a

  serializeL   :: [a] -> ByteString
  deserializeL :: ByteString -> [a]

  serializeL   = error "unimp"
  deserializeL = error "unimp"

instance Serializable Char where
  -- people will rarely want to serialize a single char,
  -- but we define them for completeness.
  serialize   x      = encodeUtf8 $ singleton x
  deserialize s      = Just $ head $ decodeUtf8 s

  -- the real use is for serializing strings.
  serializeL   = encodeUtf8 . pack
  deserializeL = unpack . decodeUtf8

instance Serializable ByteString where
    serialize   = id
    deserialize = Just

-- ...do I really need to copy everything instance of Show?
instance Serializable Int where
  serialize   = serializeL . show
  deserialize = Just . read . unpack . decodeUtf8

instance (Serializable a) => Serializable [a] where
  serialize   = serializeL
  deserialize = Just . deserializeL

-- vim: set ts=2 sw=2 et :
