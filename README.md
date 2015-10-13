EDIT: this work has since been merged into PostgREST by [this pull request](https://github.com/begriffs/postgrest/pull/295)

**Proof of concept for an idea of extending the capabilities of PostgREST**


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


**About the proposal**

After the last pull request was merged, PostgREST now support and additional parameter &select that provides the ability to shape the output (select specific columns) and cast each column if desired
```
&select=col1,col2::int,col3->>sub1::json,col3->sub1->>sub2::int
```
I propose to extend this even more to allow fetching in one request data from parent/child/related tables.
The format would look like this
```
&select=id,name,projects(id,name,tasks(id,name))
```
idea stolen from
https://developers.google.com/classroom/guides/performance#partial-response

To add filters for nested tables one would use
```
&id=eq.1&projects.tasks.id=eq.1
```

If you query only one level then the format of the GET request is exactly like it is now. Even more, the query that is executed is also identical
