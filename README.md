## SQL-parser implemented in lisp.

All data reads from `.csv` or `.tsv` or `.json` file and holds in table structure.
Table structure contain a few parameters:
* tableName - no comments
* columnNames - vector with column names (like `#("col1" "col2" …)`)
* columnIndexes - hash map where key is columns name and value is column index in the table
* data - all table data. holds as vector of vectors

SQL-parser supports next functioality:
* expressions on select. like: `select 1 + id ...`. All supported operators: `+,-,*,/,(,)`. Also can use another sql functions:
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

## How to run SQL-parser
For running this parser I use SBCL 2.0.1.
Type in your terminal:
```
git clone https://github.com/TheBestTvarynka/LispFunctionalProgramming.git
cd LispFunctionalProgramming/src
sbcl --script cli.lisp
```

## Examples
SQL-parser can execute next queries:
```
query(select distinct title, id_mp, id_fr from map_zal-skl9)
query(select distinct * from test where col > 15 and col < 23)
query(select row + 2, col, pos_x, pos_y from test)
query(select max(col) * count(row) from test)
query(select 2*(col+pos_x), substr(title, 1, 3) from test)
query(select 2*(col+pos_x), concat('piece: ', substr(title, 1, 3)) from test)
```

## Meta

Pavlo Myroniyk – @TheBestTvarynka, [pspos.developqkation@gmail.com](mailto:pspos.developqkation@gmail.com)

If you want to
* improve this project then make a pull request or write to me on email;
* report about a bug or other mistake then make an issue;
* tell me something else then write to me on email;

## Other links
Additional lisp packages that I use in this project:
* [json parser](https://github.com/hankhero/cl-json)
* [priority queue](https://github.com/dsorokin/priority-queue)

