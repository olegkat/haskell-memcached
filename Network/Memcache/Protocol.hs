-- Memcached interface.
-- Copyright (C) 2005 Evan Martin <martine@danga.com>

module Network.Memcache.Protocol (
  Server,
  connect,disconnect,stats   -- server-specific commands
) where

-- TODO:
--  - use exceptions where appropriate for protocol errors
--  - expiration time in store

import Network.Memcache
import qualified Network
import Network.Memcache.Key
import Network.Memcache.Serializable
import System.IO

-- | Gather results from action until condition is true.
ioUntil :: (a -> Bool) -> IO a -> IO [a]
ioUntil stop io = do
  val <- io
  if stop val then return []
              else do more <- ioUntil stop io
                      return (val:more)

-- | Put out a line with \r\n terminator.
hPutNetLn :: Handle -> String -> IO ()
hPutNetLn h str = hPutStr h (str ++ "\r\n")

-- | Get a line, stripping \r\n terminator.
hGetNetLn :: Handle -> IO [Char]
hGetNetLn h = do
  str <- ioUntil (== '\r') (hGetChar h)
  hGetChar h   -- read following newline
  return str

-- | Put out a command (words with terminator) and flush.
hPutCommand :: Handle -> [String] -> IO ()
hPutCommand h strs = hPutNetLn h (unwords strs) >> hFlush h

newtype Server = Server { sHandle :: Handle }

-- connect :: String -> Network.Socket.PortNumber -> IO Server
connect :: Network.HostName -> Network.PortNumber -> IO Server
connect host port = do
  handle <- Network.connectTo host (Network.PortNumber port)
  return (Server handle)

disconnect :: Server -> IO ()
disconnect = hClose . sHandle

stats :: Server -> IO [(String, String)]
stats (Server handle) = do
  hPutCommand handle ["stats"]
  statistics <- ioUntil (== "END") (hGetNetLn handle)
  return $ map (tupelize . stripSTAT) statistics where
    stripSTAT ('S':'T':'A':'T':' ':x) = x
    stripSTAT x                       = x
    tupelize line = case words line of
                      (key:rest) -> (key, unwords rest)
                      []         -> (line, "")

store :: (Key k, Serializable s) => String -> Server -> k -> s -> IO Bool
store action (Server handle) key val = do
  let flags = (0::Int)
  let exptime = (0::Int)
  let valstr = toString val
  let bytes = length valstr
  let cmd = unwords [action, toKey key, show flags, show exptime, show bytes]
  hPutNetLn handle cmd
  hPutNetLn handle valstr
  hFlush handle
  response <- hGetNetLn handle
  return (response == "STORED")

getOneValue :: Handle -> IO (Maybe String)
getOneValue handle = do
  s <- hGetNetLn handle
  case words s of
    ["VALUE", _, _, sbytes] -> do
      let count = read sbytes
      val <- sequence $ take count (repeat $ hGetChar handle)
      return $ Just val
    _ -> return Nothing

incDec :: (Key k) => String -> Server -> k -> Int -> IO (Maybe Int)
incDec cmd (Server handle) key delta = do
  hPutCommand handle [cmd, toKey key, show delta]
  response <- hGetNetLn handle
  case response of
    "NOT_FOUND" -> return Nothing
    x           -> return $ Just (read x)


instance Memcache Server where
  set     = store "set"
  add     = store "add"
  replace = store "replace"

  get (Server handle) key = do
    hPutCommand handle ["get", toKey key]
    val <- getOneValue handle
    case val of
      Nothing -> return Nothing
      Just val -> do
        hGetNetLn handle
        hGetNetLn handle
        return $ fromString val

  delete (Server handle) key delta = do
    hPutCommand handle [toKey key, show delta]
    response <- hGetNetLn handle
    return (response == "DELETED")

  incr = incDec "incr"
  decr = incDec "decr"

-- vim: set ts=2 sw=2 et :
