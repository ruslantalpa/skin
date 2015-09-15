
{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, OverloadedStrings, FlexibleContexts #-}
module Main where

import Skin.Types
import Skin.Structure
import Skin.Functions
import Skin.Error
import Skin.Parsers
--import Data.Tree
--import Data.Traversable
import Data.Maybe (fromMaybe)
-- import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
--import qualified Data.ByteString.Internal as B
--import qualified Data.List as L
import Data.String.Conversions (cs)
import Data.Bifunctor (first)
--import Text.Parsec.Error

import qualified Hasql as H
import qualified Hasql.Backend as B
import qualified Hasql.Postgres as P
import qualified Data.Vector as V


application :: String -> [Table] -> [Column] -> [Relation] -> Request -> H.Tx P.Postgres s Response
application schema tbls cols relations httpRequest  = do
    case query of
        Left e -> return $ responseLBS status200 [("Content-Type", "text/plain")] $ CL.pack e
        Right qq -> do
            let q = B.Stmt (T.pack qq) V.empty True
            row <- H.maybeEx q
            let (_, body) = fromMaybe (0::Int, Just "" :: Maybe T.Text) row
            return $ responseLBS status200 [
                  ("Content-Type", "application/json")
                , ("Debug-Query", C.pack queryStr)
                ]
                ( cs $ fromMaybe "[]" body )

    where
        queryStr = either id id query
        query = dbRequestToQuery <$> dbRequest
        dbRequest = first formatParserError request
                >>= traverse (requestNodeToQuery schema tbls cols)
                >>= (return.addRelations relations Nothing)
                >>= addJoinConditions cols
                where formatParserError = show
        request = parseGetRequest httpRequest
        -- request = buildRequest table include whereS
        -- table = T.unpack $ head $ pathInfo httpRequest
        -- include = C.unpack $ fromJust $ join $ lookup "include" $ queryString httpRequest
        -- whereS = [ (C.unpack k,C.unpack (fromMaybe "" v)) | (k,v) <- (queryString httpRequest), k `notElem` ["include"] ]

main :: IO ()
main = do
    -- db connection setup
    let pgSettings = P.ParamSettings "localhost" 5432 "skin_test" "skin_pass" "skin_test"
    poolSettings <- maybe (fail "Improper session settings") return $
        H.poolSettings 6 30
    pool :: H.Pool P.Postgres <- H.acquirePool pgSettings poolSettings

    -- read the structure of the database (all the tables in the public schema for now)
    let txParam = (Just (H.ReadCommitted, Just True))
    tblsRes <-  H.session pool $ H.tx txParam tables
    let allTables = either (fail . show) id tblsRes
    -- colsRes <-  H.session pool $ H.tx txParam $ traverse columns tbls
    -- let cols = join $ either (fail . show) id colsRes
    colsRes <-  H.session pool $ H.tx txParam columns
    let allColumns = either (fail . show) id colsRes

    relsRes <-  H.session pool $ H.tx txParam relations
    let allRelations = either (fail . show) id relsRes


    --let rels = buildRelations cols

    let schema = "2"::String

    putStrLn "Starting"
    print allRelations
    --print tbls
    --print cols
    --putStrLn $ show $ map (\(f,s,sr,_)->(f,s,sr)) rels -- debug output, the detected relations between tables

    run 3000 $ \req respond -> do
        resOrError <- liftIO $ H.session pool $ H.tx txParam $
          application schema allTables allColumns allRelations req
        either (respond . errResponse) respond resOrError
