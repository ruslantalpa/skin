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
