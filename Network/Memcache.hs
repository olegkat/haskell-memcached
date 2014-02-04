-- Memcached interface.
-- Copyright (C) 2005 Evan Martin <martine@danga.com>

module Network.Memcache where

import Network.Memcache.Serializable
import Network.Memcache.Key

class Memcache a where
  set, add, replace :: (Key k, Serializable s) => a -> k -> s -> IO Bool
  get               :: (Key k, Serializable s) => a -> k -> IO (Maybe s)
  delete            :: (Key k) => a -> k -> IO Bool
  incr, decr        :: (Key k) => a -> k -> Int -> IO (Maybe Int)

-- vim: set ts=2 sw=2 et :
