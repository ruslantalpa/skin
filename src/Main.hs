
{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, OverloadedStrings, FlexibleContexts #-}
module Main where

import Skin.Types
import Skin.Structure
import Skin.Functions
import Skin.Error
import Data.Tree
import Data.Traversable
import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Network.Wai as Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.ByteString.Internal as B
import qualified Data.List as L
import Data.String.Conversions (cs)


import qualified Hasql as H
import qualified Hasql.Backend as B
import qualified Hasql.Postgres as P
import qualified Data.Vector as V

application :: [Table] -> [Column] -> [RelationEntry] -> Wai.Request -> H.Tx P.Postgres s Wai.Response
application tables columns relations httpRequest  = do
    case query of
        Nothing -> return $ Wai.responseLBS status200 [("Content-Type", "text/plain")] $ CL.pack "Invalid request"
        Just qq -> do
            let q = B.Stmt (T.pack qq) V.empty True
            row <- H.maybeEx q
            let (queryTotal, body) = fromMaybe (0::Int, Just "" :: Maybe T.Text) row
            return $ Wai.responseLBS status200 [
                  ("Content-Type", "application/json")
                , ("Debug-Query", C.pack queryStr)
                ]
                ( cs $ fromMaybe "[]" body )

    where
        queryStr = fromMaybe "Invalid request" query
        query = dbRequestToQuery <$> dbRequest
        dbRequest = addJoinConditions
                =<< (return.addRelations relations Nothing)
                =<< eitherToMaybe (traverse <$> pure (requestNodeToQuery tables columns) <*> request)
            where
                eitherToMaybe :: Either b (Maybe a) -> Maybe a
                eitherToMaybe = either (const Nothing) id
        request = buildRequest table include whereS
        table = T.unpack $ head $ Wai.pathInfo httpRequest
        include = C.unpack $ fromJust $ join $ lookup "include" $ Wai.queryString httpRequest
        whereS = [ (C.unpack k,C.unpack (fromMaybe "" v)) | (k,v) <- (Wai.queryString httpRequest), k `notElem` ["include"] ]

main = do
    -- db connection setup
    let pgSettings = P.ParamSettings "localhost" 5432 "skin_test" "skin_pass" "skin_test"
    poolSettings <- maybe (fail "Improper session settings") return $
        H.poolSettings 6 30
    pool :: H.Pool P.Postgres <- H.acquirePool pgSettings poolSettings

    -- read the structure of the database (all the tables in the public schema for now)
    let txParam = (Just (H.ReadCommitted, Just True))
    tblsRes <-  H.session pool $ H.tx txParam tables
    let tbls = either (fail . show) id tblsRes
    colsRes <-  H.session pool $ H.tx txParam $ traverse columns tbls
    let cols = join $ either (fail . show) id colsRes
    let rels = buildRelations cols

    putStrLn "Starting"
    putStrLn $ show $ map (\(f,s,sr,r)->(f,s,sr)) rels -- debug output, the detected relations between tables

    run 3000 $ \req respond -> do
        resOrError <- liftIO $ H.session pool $ H.tx (Just (H.ReadCommitted, Just True)) $
          application tbls cols rels req
        either (respond . errResponse) respond resOrError
