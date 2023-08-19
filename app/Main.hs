{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Servant
import Network.Wai.Handler.Warp
import qualified Network.Wai.Middleware.RequestLogger as Log
import Network.Wai
import Prelude
import qualified News as N
import Service (CacheRef, initCache)

main :: IO ()
main =
    initCache >>= run 8081 . Log.logStdout . app

server :: CacheRef -> Server N.NewsApis
server = N.server

api :: Proxy N.NewsApis
api = Proxy

app :: CacheRef -> Application
app cache = serve api $ server cache