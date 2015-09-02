----- SCHEMA ----
CREATE TABLE clients(
    id INT PRIMARY KEY     NOT NULL,
    name           TEXT    NOT NULL
);
CREATE TABLE projects(
    id INT PRIMARY KEY     NOT NULL,
    name           TEXT    NOT NULL,
    client_id      INT     REFERENCES clients(id)
);
CREATE TABLE tasks(
    id INT PRIMARY KEY     NOT NULL,
    name           TEXT    NOT NULL,
    project_id      INT     REFERENCES projects(id)
);
CREATE TABLE users(
    id INT PRIMARY KEY     NOT NULL,
    name           TEXT    NOT NULL
);
CREATE TABLE users_tasks(
    user_id      INT     REFERENCES users(id),
    task_id      INT     REFERENCES tasks(id),
    CONSTRAINT task_user PRIMARY KEY (task_id,user_id)
);
CREATE TABLE users_projects(
    user_id         INT     REFERENCES users(id),
    project_id      INT     REFERENCES projects(id),
    CONSTRAINT project_user PRIMARY KEY (project_id, user_id)
);

------- SAMPLE DATA -----
INSERT INTO clients VALUES (1, 'Microsoft'),(2, 'Apple');
INSERT INTO projects VALUES (1,'Windows 7', 1),(2,'Windows 10', 1),(3,'IOS', 2),(4,'OSX', 2);
INSERT INTO tasks VALUES (1,'Design w7',1),(2,'Code w7',1),(3,'Design w10',2),(4,'Code w10',2),(5,'Design IOS',3),(6,'Code IOS',3),(7,'Design OSX',4),(8,'Code OSX',4);
INSERT INTO users VALUES (1, 'Angela Martin'),(2, 'Michael Scott'),(3, 'Dwight Schrute');
INSERT INTO users_projects VALUES(1,1),(1,2),(2,3),(2,4),(3,1),(3,3);
INSERT INTO users_tasks VALUES(1,1),(1,2),(1,3),(1,4),(2,5),(2,6),(2,7),(2,8),(3,1),(3,5);


--------- example queris that can return tree structures -------------
SELECT
    pg_catalog.count(t),
  array_to_json(array_agg(row_to_json(t)))::CHARACTER VARYING AS json
FROM (
    WITH clients AS (
      SELECT clients.id, clients.name FROM clients
    )
    SELECT
        projects.id, projects.name, projects.client_id,
        (
            SELECT array_to_json(array_agg(row_to_json(tasks)))
            FROM (
                SELECT tasks.id, tasks.name, tasks.project_id
                FROM tasks
                WHERE tasks.project_id = projects.id
            ) tasks
        ) AS tasks,
        (
            SELECT array_to_json(array_agg(row_to_json(users)))
            FROM (
                SELECT users.id, users.name
                FROM users, users_projects
                WHERE users.id = users_projects.user_id AND projects.id = users_projects.project_id
            ) users
        ) AS users,
        row_to_json(clients.*) AS client
    FROM projects
    INNER JOIN clients ON projects.client_id = clients.id
) t;


WITH clients AS (
  SELECT clients.id, clients.name FROM clients
)
SELECT
    projects.id, projects.name, projects.client_id,
    (
        SELECT array_to_json(array_agg(row_to_json(tasks)))
        FROM (
            SELECT tasks.id, tasks.name, tasks.project_id
            FROM tasks
            WHERE tasks.project_id = projects.id
        ) tasks
    ) AS tasks,
    (
        SELECT array_to_json(array_agg(row_to_json(users)))
        FROM (
            SELECT users.id, users.name
            FROM users
            INNER JOIN users_projects ON
                users_projects.user_id = users.id AND
                users_projects.project_id = projects.id
        ) users
    ) AS users,
    row_to_json(clients.*) AS client
FROM projects
INNER JOIN clients ON projects.client_id = clients.id

WITH clients AS (
  SELECT clients.id, clients.name FROM clients
)
SELECT
    projects.id, projects.name, projects.client_id,
    (
        SELECT array_to_json(array_agg(row_to_json(tasks)))
        FROM (
            SELECT tasks.id, tasks.name, tasks.project_id
            FROM tasks
            WHERE tasks.project_id = projects.id
        ) tasks
    ) AS tasks,
    (
        SELECT array_to_json(array_agg(row_to_json(users)))
        FROM (
            SELECT users.id, users.name
            FROM users, users_projects
            WHERE
                users_projects.user_id = users.id AND
                users_projects.project_id = projects.id
        ) users
    ) AS users,
    row_to_json(clients.*) AS client
FROM projects, clients
WHERE projects.client_id = clients.id
