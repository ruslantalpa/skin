module Skin.Structure
(
  getColumns
, getTables
, getRelations
)
where

import Skin.Types
import Data.Maybe
import GHC.Exts (groupWith)

getColumns = columns
getTables = tables
getRelations = buildRelations getColumns

buildRelations :: [Column] -> [RelationEntry]
buildRelations columns = sRel ++ mRel
    where sRel = simpleRelations columns
          mRel = linkRelations sRel

          linkRelations :: [RelationEntry] -> [RelationEntry]
          linkRelations relations = concatMap link2Relation links
                where links = filter (\g ->length g == 2 && all (\(f,s,rs,r)->rs=="child") g) $ groupWith (\(f,s,rs,r)->f) relations
                      link2Relation link = [(t1,t2,"many", Many r1 r2),(t2,t1,"many", Many r1 r2)]
                          where linktbl = head $ map (\(f,s,rs,r)->f) link
                                [(t1,Child r1),(t2,Child r2)] = map (\(f,s,rs,r)->(s,r)) link

          simpleRelations :: [Column]->[RelationEntry]
          simpleRelations = concatMap (\column@(Column {colTable=table, colFk=Just (ForeignKey {fkTable=fTable})})->[(table, fTable, "child", Child column),(fTable, table, "parent", Parent column)])
                            . filter (isJust.colFk)


------------ Hardcoded DB schema ----------------
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
