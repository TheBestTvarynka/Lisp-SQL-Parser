## SQL-parser implemented in lisp.

All data reads from `csv` or `.tsv` or `.json` file and holds in table structure.
Table structure contain a few parameters:
* tableName - no comments
* columnNames - vector with column names (like `#("col1" "col2" …)`)
* columnIndexes - hash map where key is columns name and value is column index in the table
* data - all table data. holds as vector of vectors

SQL-parser supports next functioality:
* expressions on select. like: `select 1 + id ...`. All supported operators: `+,-,*,/,(,)`. Also can use another functions:
    - substr (str, from, number_of_char)
    - concat (str1, str2)
* aggregate functions: count(), avg(), max()
* ~~case~~ in progress...
* from
* ~~joins (inner, left, right, full outer)~~ in progress...
* order by. can on multiply columns, like: `order by col1, col2 desc`
* where. can use `and` and `or` but without `()`. Only `=`, `<`, `>` in conditions.
* ~~group by~~ in progress...
* ~~having~~ in progress...

## Examples
SQL-parser can execute next queries:
`query(select distinct title, id_mp, id_fr from map_zal-skl9)`
`query(select distinct * from test where col > 15 and col < 23)`
`query(select row + 2, col, pos_x, pos_y from test)`
`query(select max(col) * count(row) from test)`
`query(select 2*(col+pos_x), substr(title, 1, 3) from test)`
`query(select 2*(col+pos_x), concat('piece: ', substr(title, 1, 3)) from test)`

## Meta

Pavlo Myroniyk – @TheBestTvarynka, [pspos.developqkation@gmail.com](mailto:pspos.developqkation@gmail.com)

## Other links
Additional lisp packages that I use in this project:
* [json parser](https://github.com/hankhero/cl-json)
* [priority queue](https://github.com/dsorokin/priority-queue)

