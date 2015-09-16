
{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, OverloadedStrings, FlexibleContexts #-}
module Main where

import Skin.Types
import Skin.Structure
import Skin.Functions
import Skin.Error
import Skin.Parsers
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (liftIO)
import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import Data.String.Conversions (cs)
import Data.Bifunctor (first)

import qualified Hasql as H
import qualified Hasql.Backend as B
import qualified Hasql.Postgres as P
import qualified Data.Vector as V


application :: String -> [Table] -> [Column] -> [Relation] -> Request -> H.Tx P.Postgres s Response
application schema tbls cols relations httpRequest  = do
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
                >>= traverse (requestNodeToQuery schema tbls cols)
                >>= (return.addRelations relations Nothing)
                >>= addJoinConditions cols
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
