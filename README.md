## SQL-parser implemented in lisp.

All data reads from `.csv` or `.tsv` or `.json` file and holds in table structure.
Table structure contain a few parameters:
* tableName - no comments
* columnNames - vector with column names (like `#("col1" "col2" …)`)
* columnIndexes - hash map where key is columns name and value is column index in the table
* data - all table data. holds as vector of vectors

SQL-parser supports next functioality:
* expressions in select. like: `select 1 + id * ...`. All supported operators: `+,-,*,/,(,)`.
* sql functions:
    - substr (str, from, number_of_char)
    - concat (str1, str2)
* as operator for renaming columns. `select count(col) as 'count' from …`. Unlike regular SQL, here we must enclose new column name between simple quotes `'new_column_name'`.
* aggregate functions: count(), avg(), max()
* ~~case~~ in progress...
* from
* joins (inner, left, right, full outer).
* order by. can use with multiply columns, like: `order by col1, col2 desc`
* where. can use `and` and `or` but without `()`. Only `=`, `<`, `>` inside a condition.
* group by
* having (but only one condition)
* union
* limit

## How to run SQL-parser
For running this parser I use SBCL 2.0.1.
Type in your terminal:
```bash
git clone https://github.com/TheBestTvarynka/Lisp-SQL-Parser.git
cd Lisp-SQL-Parser/src
sbcl --script cli.lisp
```

## Examples
SQL-parser can execute next queries:
```sql
query(select 5 + 6 * 2)
query(select 5*(6+2))
query(select concat('FirstName', substr('Myroniuk Pavlo Yaroslavovych', 8, 6)))
query(select distinct title, id_mp, id_fr from map_zal-skl9)
query(select distinct * from test where col > 15 and col < 23)
query(select row + 2, col, pos_x, from test)
query(select max(col) * count(row) from test)
query(select 2*(col+pos_x), substr(title, 1, 3) from test)
query(select 2*(col+pos_x), concat('piece: ', substr(title, 1, 3)) from test)
query(select test2.id, test2.price, test2.owner, test.row, test.title from test2 left join test on test.id = test2.id)
query(select test2.id, test2.price, test2.owner, test.row, test.title from test2 left join test on test.id = test2.id order by test2.price)
query(select test2.id, test2.price, test2.owner, test.row, test.title from test2 left join test on test.id = test2.id where test2.id < 5 order by test2.price desc)
query(select test2.id, test2.price, test2.owner, test.row, test.col, test.pos_x, test.title from test2 inner join test on test.id = test2.id)
query(select test.id, test.row, test.col, test.title, test2.id, test2.price, test2.owner from test full outer join test2 on test.id = test2.id)
query(select id, price, owner from test2 union select id, price, owner from test3)
query(select id, price, owner from test2 union select id, price, owner from test3 union select id, price, owner from test4)
query(select 1 union select 2 union select 3 union select 4)
query(select row, count(col) as 'col_count', count(title) as 'title_count' from test group by row)
query(select row, count(col) as 'count', max(pos_x) as 'max' from test group by row)
query(select row, count(col) from test group by row having count(col) > 3)
query(select row, count(col) from test group by row having count(col) = 4)
query(select row, count(col) from test group by row having count(col) < 5)
query(select row, count(col) from test group by row having count(col) > 3 order by row desc)
query(select row, col, title from test limit 5)
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

