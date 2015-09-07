
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Skin.Types
import Skin.Structure
import Skin.Functions
import Data.Tree
import Data.Traversable
import Data.Maybe
import Control.Monad
import qualified Network.Wai as Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.ByteString.Internal as B
import qualified Data.List as L

application httpRequest respond = respond $ Wai.responseLBS status200 [("Content-Type", "text/plain")] $ CL.pack queryStr
    where
        queryStr = fromMaybe "Invalid request" query
        query = dbRequestToQuery <$> dbRequest
        dbRequest = addJoinConditions
                =<< (return.addRelations allRelations Nothing)
                =<< eitherToMaybe (traverse <$> pure (requestNodeToQuery tables columns) <*> request)
            where
                allRelations = buildRelations columns
                eitherToMaybe :: Either b (Maybe a) -> Maybe a
                eitherToMaybe = either (const Nothing) id
        request = buildRequest table include whereS
        table = T.unpack $ head $ Wai.pathInfo httpRequest
        include = C.unpack $ fromJust $ join $ lookup "include" $ Wai.queryString httpRequest
        whereS = [ (C.unpack k,C.unpack (fromMaybe "" v)) | (k,v) <- (Wai.queryString httpRequest), k `notElem` ["include"] ]
        columns = getColumns
        tables = getTables

main = run 3000 application
