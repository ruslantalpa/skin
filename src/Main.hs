
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Skin.Types
import Skin.Functions
import Data.Tree
import Data.Traversable
import Data.Maybe
import Control.Monad
import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.ByteString.Internal as B

clientsFk = ForeignKey "clients" "id"
projectsFk = ForeignKey "projects" "id"
usersFk = ForeignKey "users" "id"
tasksFk = ForeignKey "tasks" "id"

clientsTable = Table "clients"
clientsIdColumn = Column "clients" "id" Nothing
clientsNameColumn = Column "clients" "name" Nothing

projectsTable = Table "projects"
projectsIdColumn = Column "projects" "id" Nothing
projectsNameColumn = Column "projects" "name" Nothing
projectsClientIdColumn = Column "projects" "client_id" (Just clientsFk)


tasksTable = Table "tasks"
tasksIdColumn = Column "tasks" "id" Nothing
tasksNameColumn = Column "tasks" "name" Nothing
tasksProjectIdColumn = Column "tasks" "project_id" (Just projectsFk)

usersTable = Table "users"
usersIdColumn = Column "users" "id" Nothing
usersNameColumn = Column "users" "name" Nothing

usersProjectsTable = Table "users_projects"
upUserIdColumn = Column "users_projects" "user_id" (Just usersFk)
upProjectIdColumn = Column "users_projects" "project_id" (Just projectsFk)

usersTasksTable = Table "users_tasks"
utUserIdColumn = Column "users_tasks" "user_id" (Just usersFk)
utTaskIdColumn = Column "users_tasks" "task_id" (Just tasksFk)

tables = [clientsTable, projectsTable, usersTable, tasksTable, usersProjectsTable, usersTasksTable]
columns = [
  clientsIdColumn, clientsNameColumn, projectsIdColumn, projectsNameColumn, projectsClientIdColumn,
  usersIdColumn, usersNameColumn, tasksIdColumn, tasksNameColumn, tasksProjectIdColumn,
  upUserIdColumn, upProjectIdColumn, utUserIdColumn, utTaskIdColumn
  ]


application httpRequest respond = respond $ responseLBS status200 [("Content-Type", "text/plain")] $ CL.pack queryStr
    where
        queryStr = fromMaybe "Invalid request" query
        query = dbRequestToQuery <$> dbRequest
        dbRequest = traverse (requestNodeToQuery tables columns) request
                >>= (return.addRelations allRelations Nothing)
                >>= addJoinConditions
            where allRelations = buildRelations columns
        request = buildRequest table include
        table = T.unpack $ head $ pathInfo httpRequest
        include = C.unpack $ fromJust $ join $ lookup "include" $ queryString httpRequest

main = run 3000 application
