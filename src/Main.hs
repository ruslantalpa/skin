
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad.IO.Class     (liftIO)
import           Data.Bifunctor             (first)
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Lazy.Char8 as CL
import           Data.Maybe                 (fromMaybe)
import           Data.String.Conversions    (cs)
import qualified Data.Text                  as T
import           Network.HTTP.Types         (status200)
import           Network.Wai
import           Network.Wai.Handler.Warp   (run)
import           Skin.Error
import           Skin.Functions
import           Skin.Parsers
import           Skin.Structure
import           Skin.Types

import qualified Data.Vector                as V
import qualified Hasql                      as H
import qualified Hasql.Backend              as B
import qualified Hasql.Postgres             as P


application :: String -> [Table] -> [Column] -> [Relation] -> Request -> H.Tx P.Postgres s Response
application schema allTables allColumns allRelations httpRequest  = do
    -- return $ responseLBS status200 [("Content-Type", "text/plain")] $ CL.pack $ show apiRequest
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
        dbRequest = first formatParserError apiRequest
                >>= traverse (requestNodeToQuery schema allTables allColumns)
                >>= addRelations allRelations Nothing
                >>= addJoinConditions allColumns
                where formatParserError = show
        apiRequest = parseGetRequest httpRequest

main :: IO ()
main = do
    let schema = "2"::String

    -- db connection setup
    let pgSettings = P.ParamSettings "localhost" 5432 "skin_test" "skin_pass" "skin_test"
    poolSettings <- maybe (fail "Improper session settings") return $ H.poolSettings 6 30
    pool :: H.Pool P.Postgres <- H.acquirePool pgSettings poolSettings

    -- read the structure of the database
    let txParam = (Just (H.ReadCommitted, Just True))
    tblsRes <-  H.session pool $ H.tx txParam tables
    let allTables = either (fail . show) id tblsRes
    colsRes <-  H.session pool $ H.tx txParam columns
    let allColumns = either (fail . show) id colsRes
    relsRes <-  H.session pool $ H.tx txParam relations
    let allRelations = either (fail . show) id relsRes

    putStrLn "Starting"
    print allRelations

    run 3000 $ \req respond -> do
        resOrError <- liftIO $ H.session pool $ H.tx txParam $
          application schema allTables allColumns allRelations req
        either (respond . errResponse) respond resOrError
