{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module News where

import Servant
import GHC.Generics
import Data.Text
import Control.Monad.IO.Class (liftIO)
import Data.Maybe
import Service
import Text.Read (readMaybe)

type NewsApis =
    ("news" :> "search"
        :> QueryParam "keyword" Text
        :> QueryParam "title" Text
        :> QueryParam "author" Text
        :> Get '[JSON] ServiceResponse)
    :<|> ("news" :> "top" :> QueryParam "count" Text
            :> Get '[JSON] ServiceResponse
        )

searchNews :: CacheRef -> Maybe Text -> Maybe Text -> Maybe Text -> Handler ServiceResponse
searchNews cache keyword title author =
    liftIO $ transformResult <$> serviceCall request cache
    where
        request  = ServiceRequest title author keyword (Just 1) (pack "SEARCH")

fetchTopNews :: CacheRef -> Maybe Text -> Handler ServiceResponse
fetchTopNews cache count =
    liftIO $ transformResult <$> serviceCall request cache
    where
        request = ServiceRequest Nothing Nothing Nothing getCount $ pack "TOP_ITEMS"
        getCount = readMaybe . unpack =<< count
    
transformResult :: Either ServiceError ServiceResponse -> ServiceResponse
transformResult (Left _)  = ServiceResponse []
transformResult (Right r) = r

server :: CacheRef -> Server NewsApis
server  a = searchNews a :<|> fetchTopNews a