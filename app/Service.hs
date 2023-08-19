{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Service where

import Network.HTTP.Simple
import Network.HTTP.Simple (Request(..))
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Aeson as A
import Data.Aeson.TH
import Prelude
import Data.Text
import Data.Maybe
import Control.Exception(SomeException, try, displayException)
import Data.Functor (($>))
import Data.Text.Encoding(encodeUtf8)
import System.Environment (lookupEnv)
import System.IO.Unsafe(unsafePerformIO)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as BS
import qualified Data.Cache.LRU as LRU
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef)
import GHC.Types (Any)
import System.Posix (EpochTime, epochTime)
import Unsafe.Coerce (unsafeCoerce)
import Text.Read (readMaybe)

type CacheRef =  IORef (LRU.LRU Text ServiceCache)

data ServiceCache = ServiceCache
  { createTime :: EpochTime
  , entry :: Any
  , ops :: Text
  }

data ServiceRequest = ServiceRequest {
    mTitle :: Maybe Text,
    mAuthor :: Maybe Text,
    mContains :: Maybe Text,
    mCount :: Maybe Int,
    ops :: Text
    }

data ServiceResponse = ServiceResponse {
    articles :: [Article]
}

data Article = Article {
    title :: Text
    , description :: Text
    , content :: Text
    , url :: Text
    , image :: Text
    , source :: Source
}

data Source = Source { name :: Text, url :: Text}

data ServiceError = ServiceError {code :: Int , errorMessage :: String}

deriveJSON defaultOptions ''Source
deriveJSON defaultOptions ''Article
deriveJSON defaultOptions ''ServiceResponse

initCache :: IO CacheRef
initCache = newIORef $ LRU.newLRU (Just 100)

serviceCall :: ServiceRequest -> CacheRef -> IO (Either ServiceError ServiceResponse)
serviceCall req@(ServiceRequest title author contains count op) cache =
    readFromCache
      >>= maybe (callNewsService req >>= either (pure . Left) writeToCache) (pure . Right)
    where
        key = let fn | op == (pack "SEARCH") = (pack "SEARCH_") <> (intercalate (pack "_") $ catMaybes [title, author, contains])
                     | otherwise = pack $ maybe "TOP" ((<>) "TOP" . show) count
              in fn

        writeToCache response = do
            ct <- epochTime
            let value = ServiceCache ct (unsafeCoerce response) op
            insertIntoCache value $> (Right response)

        readFromCache = do
          lru <- readIORef cache
          case (snd $ LRU.lookup key lru) of
            Just (ServiceCache createTime entry _) -> do
                ct <- epochTime
                if (ct - createTime) < cacheTimeLimit
                    then pure $ Just $ unsafeCoerce entry
                    else do
                        deleteFromCache $> Nothing
            Nothing -> pure Nothing
        
        deleteFromCache =
            let modifiedLru lru = (fst $ LRU.delete key lru, ())
              in atomicModifyIORef' cache modifiedLru
        
        insertIntoCache value =
            let insertLru lru = (LRU.insert key value lru, ())
              in atomicModifyIORef' cache insertLru
        
        cacheTimeLimit = fromMaybe 7200 $ readMaybe =<< (unsafePerformIO $ lookupEnv "CACHE_TTL") -- default cache ttl is 2 hours

          
callNewsService :: ServiceRequest -> IO (Either ServiceError ServiceResponse)
callNewsService req@(ServiceRequest _ _ _ _ op) = do
    baseReq  <- parseRequest $ serviceUrl (unpack op)
    response <- try $! httpLBS $ modifyRequest baseReq
    case response of
        Left (e :: SomeException) ->
            putStrLn ("error occured while calling service " ++ (displayException e))
                $> (Left $ ServiceError 500 "Unexpected Error")
        Right res -> do
            putStrLn ("Success Response")
            let status = getResponseStatusCode res
              in maybe (pure $ handleError status) (pure .Right) (A.decodeStrict . BS.toStrict . getResponseBody $ res) 
    where
    handleError 429 = Left $ ServiceError 429 "Too Many Requests"
    handleError 400 = Left $ ServiceError 400 "Bad Request"
    handleError _   = Left $ ServiceError 408 "Upstream Error"

    modifyRequest =
        setRequestQueryString (getQueryString req) . setRequestMethod (B.pack "GET")
    
    getApiKeyHeader = fromMaybe "temp_key" (unsafePerformIO $ lookupEnv "SERVICE_API_KEY")
    
    getQueryString (ServiceRequest mTitle mAuthor mContains mCount _) =
        let q = intercalate (pack " AND ") $ catMaybes [mTitle, mAuthor, mContains]
        in [(B.pack "q" , Just $ encodeUtf8 q)
            , (B.pack "apikey" , Just $ B.pack getApiKeyHeader)
            , (B.pack "lang", Just $ B.pack "en")
            , (B.pack "max" , B.pack . show <$> mCount)
            ] -- add in query
    
    serviceUrl "SEARCH" = "https://gnews.io/api/v4/search"
    serviceUrl _        = "https://gnews.io/api/v4/top-headlines?category=general"