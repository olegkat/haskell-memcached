Memcached interface.
Copyright (C) 2005 Evan Martin <martine@danga.com>

This program demonstrates the main use of the haskell-memcached API.
It's runnable directly if you have a server running on localhost:11211.

We first import the "Memcache" type class directly as well as all the
functions qualified.  The latter two imports will be explained below.

> import Network.Memcache(Memcache)
> import qualified Network.Memcache
> import Network.Memcache.Protocol as Single
> import Network.Memcache.Serializable(Serializable(..))

For this demonstration, we only use a single server.  But working with
a server pool ought to be transparent, as both single servers and the
pool are instances of the "Memcache" type class.

> main = do
>   server <- Single.connect "localhost" 11211
>   simpleDemo server
>   serializeDemo server
>   Single.disconnect server

> simpleDemo :: (Memcache mc) => mc -> IO ()
> simpleDemo memcache = do

When setting/getting keys, Haskell must be able to infer their type.
Typically, context will determine this, but if it doesn't you need
to annotate.

>   let foo = 3 :: Int
>   success <- Network.Memcache.set memcache "foo" foo
>   putStrLn ("Setting foo => 3: " ++ show success ++ ".")

Similarly for "get":
Generally this won't be a problem (the way you use the value will
make it specific) but it means that a naive "get" followed by a
"print" won't work -- there's no way to know whether you were trying
to get an Int or a String.
 
>   foo' <- Network.Memcache.get memcache "foo"
>   case foo' of
>     Nothing -> putStrLn "Retrieving foo: expired from cache?"
>     Just v  -> putStrLn ("Cached value for foo is " ++ show (v::Int) ++ ".")



By implementing the "serializable" class, you can serialize more complicated
data structures directly.  Suppose we had a "User" record that contained
information about a user that we wanted to be able to retrieve quickly.

> data User = User {
>   username :: String,
>   fontsize :: Int
> } deriving Show

For this simple type we can stringify it as just "username fontsize".
For more complicated data, you can do whatever crazy bitpacking necessary.

> instance Serializable User where
>   serialize (User username fontsize) = username ++ " " ++ (show fontsize)
>   deserialize str = case words str of
>                      (a:b:[]) -> Just (User a (read b))
>                      _        -> Nothing

> serializeDemo :: (Memcache mc) => mc -> IO ()
> serializeDemo memcache = do
>   let fred = User "fred" 24    -- fred likes large fonts
>   Network.Memcache.set memcache "u:fred" fred

>   fred' <- Network.Memcache.get memcache "u:fred"
>   putStrLn ("Fred is " ++ show (fred' :: Maybe User))

>   invalid <- Network.Memcache.get memcache "this key doesn't exist"
>   putStrLn ("Unknown returns: " ++ show (invalid :: Maybe User))


vim: set ts=2 sw=2 et :
