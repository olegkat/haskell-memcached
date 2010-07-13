-- Memcached interface.
-- Copyright (C) 2005 Evan Martin <martine@danga.com>

module Network.Memcache.Key(Key, hash, toKey) where

import Data.List(foldl')

-- A Memcached key must be hashable (so it can be deterministically distributed
-- across multiple servers) and convertable to a string (as that's what
-- Memcached uses).

class Key a where
  hash     :: a -> Int
  toKey    :: a -> String

-- I really just want to make String an instance of Key,
-- but this is the best I can figure out.
class KeyElem a where
  num :: a -> Int
  chr :: a -> Char
instance KeyElem Char where
  num = fromEnum
  chr = id
instance (KeyElem a) => Key [a] where
  -- glib's string hash: fast and good for short strings
  hash  = foldl' (\h i -> 31*h + i) 0 . map num
  toKey = map chr

-- vim: set ts=2 sw=2 et :
