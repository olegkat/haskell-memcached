-- Memcached interface.
-- Copyright (C) 2005 Evan Martin <martine@danga.com>

module Network.Memcache.Serializable(Serializable, toString, fromString) where

-- It'd be nice to use "show" for serialization, but when we
-- serialize a String we want to serialize it without the quotes.

-- TODO:
--  - allow serializing bytes as Ptr
--    to do this, add a "putToHandle", etc. method in Serializable
--    where the default uses toString, but for Ptr uses socket stuff.

--import Foreign.Marshal.Utils
--import Foreign.Storable (Storable, sizeOf)

class Serializable a where
  toString    :: a -> String
  fromString  :: String -> Maybe a

  toStringL   :: [a] -> String
  fromStringL :: String -> [a]

  toStringL   = error "unimp"
  fromStringL = error "unimp"

instance Serializable Char where
  -- people will rarely want to serialize a single char,
  -- but we define them for completeness.
  toString   x      = [x]
  fromString (c:[]) = Just c
  fromString _      = Nothing

  -- the real use is for serializing strings.
  toStringL   = id
  fromStringL = id

-- ...do I really need to copy everything instance of Show?
instance Serializable Int where
  toString   = show
  fromString = Just . read

instance (Serializable a) => Serializable [a] where
  toString   = toStringL
  fromString = Just . fromStringL

-- vim: set ts=2 sw=2 et :
