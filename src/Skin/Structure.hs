module Skin.Structure
(
  getColumns
, getTables
)
where

import Skin.Types
getColumns = columns
getTables = tables
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
