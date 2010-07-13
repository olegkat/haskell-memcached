-- Memcached interface.
-- Copyright (C) 2005 Evan Martin <martine@danga.com>

module Main where

import qualified Network.Memcache
import Network.Memcache.Key(hash)
import qualified Network.Memcache.Protocol as S

import Control.Exception
import System.Process
import System.IO   -- used for emulating sleep()
import Test.HUnit

withServerConnection :: (S.Server -> IO ()) -> IO ()
withServerConnection f = bracket connect disconnect f where
  connect = S.connect "localhost" 11211
  disconnect = S.disconnect

statsTest :: Test
statsTest = TestCase $ withServerConnection $ \server -> do
  stats <- S.stats server
  assertBool "stats returns multiple stats" (length stats > 10)

setGetTest :: Test
setGetTest = TestCase $ withServerConnection $ \server -> do
  let foo = 3 :: Int
  success <- Network.Memcache.set server "foo" foo
  foo' <- Network.Memcache.get server "foo"
  case foo' of
    Nothing -> assertFailure "'foo' not found just after setting it"
    Just v  -> assertEqual "foo value" (3 :: Int) v

hashTest :: Test
hashTest = TestCase $ do
  assertBool "hash produces different values" (hash key1 /= hash key2)
  where key1 = "foo"; key2 = "bar"

-- XXX hack: is there no other way to wait?
sleep :: Int -> IO ()
sleep x = hWaitForInput stdin x >> return ()

main :: IO ()
main = bracket upDaemon downDaemon runTests >> return () where
  upDaemon   = do m <- runCommand "memcached"
                  sleep 200  -- give it time to start up and bind.
                  return m
  downDaemon = terminateProcess
  runTests _ = runTestTT $ TestList [statsTest, setGetTest, hashTest]

-- vim: set ts=2 sw=2 et :
