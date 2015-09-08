Clone & build & run
```
git clone https://github.com/ruslantalpa/skin.git
cd skin
stack build
psql -f ./schema.sql
stack exec skin
```
Open one of the urls in the browser (or curl)
```
http://localhost:3000/projects?id=lt.4&include=id,name,tasks(id,name),clients(id,name),users(id,name)
http://localhost:3000/projects?id=lt.4&tasks.id=eq.1&include=id,name,tasks(id,name)
http://localhost:3000/clients?id=eq.1&include=id,name,projects(id,name,tasks(id,name))
```
