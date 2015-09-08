CREATE USER skin_test WITH PASSWORD 'skin_pass';
ALTER ROLE skin_test WITH NOSUPERUSER INHERIT NOCREATEROLE NOCREATEDB NOREPLICATION;

CREATE DATABASE skin_test OWNER skin_test;

\connect skin_test

CREATE TABLE clients(
    id INT PRIMARY KEY     NOT NULL,
    name           TEXT    NOT NULL
);
ALTER TABLE clients OWNER TO skin_test;

CREATE TABLE projects(
    id INT PRIMARY KEY     NOT NULL,
    name           TEXT    NOT NULL,
    client_id      INT     REFERENCES clients(id)
);
ALTER TABLE projects OWNER TO skin_test;

CREATE TABLE tasks(
    id INT PRIMARY KEY     NOT NULL,
    name           TEXT    NOT NULL,
    project_id      INT     REFERENCES projects(id)
);
ALTER TABLE tasks OWNER TO skin_test;

CREATE TABLE users(
    id INT PRIMARY KEY     NOT NULL,
    name           TEXT    NOT NULL
);
ALTER TABLE users OWNER TO skin_test;

CREATE TABLE users_tasks(
    user_id      INT     REFERENCES users(id),
    task_id      INT     REFERENCES tasks(id),
    CONSTRAINT task_user PRIMARY KEY (task_id,user_id)
);
ALTER TABLE users_tasks OWNER TO skin_test;

CREATE TABLE users_projects(
    user_id         INT     REFERENCES users(id),
    project_id      INT     REFERENCES projects(id),
    CONSTRAINT project_user PRIMARY KEY (project_id, user_id)
);
ALTER TABLE users_projects OWNER TO skin_test;


------- SAMPLE DATA -----
INSERT INTO clients VALUES (1, 'Microsoft'),(2, 'Apple');
INSERT INTO projects VALUES (1,'Windows 7', 1),(2,'Windows 10', 1),(3,'IOS', 2),(4,'OSX', 2);
INSERT INTO tasks VALUES (1,'Design w7',1),(2,'Code w7',1),(3,'Design w10',2),(4,'Code w10',2),(5,'Design IOS',3),(6,'Code IOS',3),(7,'Design OSX',4),(8,'Code OSX',4);
INSERT INTO users VALUES (1, 'Angela Martin'),(2, 'Michael Scott'),(3, 'Dwight Schrute');
INSERT INTO users_projects VALUES(1,1),(1,2),(2,3),(2,4),(3,1),(3,3);
INSERT INTO users_tasks VALUES(1,1),(1,2),(1,3),(1,4),(2,5),(2,6),(2,7),(2,8),(3,1),(3,5);
