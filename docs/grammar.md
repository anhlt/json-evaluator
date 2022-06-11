## Lexical
```
NUMBER         -> DIGIT+ ( "." DIGIT+ )? ;
STRING         -> "\"" <any char except "\"">* "\"" ;
IDENTIFIER     -> ALPHA ( ALPHA | DIGIT )* ;
ALPHA          -> "a" ... "z" | "A" ... "Z" | "_" ;
DIGIT          -> "0" ... "9" ;
```


## Expression
```
type            -> "LIST" "[" type "]" 
                | simpletype;

simpletype      -> INT 
                | STRING
                | DATE
                | DATETIME ;

columnDeclare   -> INDENTIFIER types "VALUES" "(" STRING ")" ;
columnsDeclare  -> columnDeclare ("," columnDeclare)* ;
```

## Statements



```
statement      -> declareStmt
                | selectStmt

declareStmt    -> "CREATE" "TABLE" IDENTIFIER (columnsDeclare) ";" ;


```