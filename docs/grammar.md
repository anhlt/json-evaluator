## Lexical
```
NUMBER         -> DIGIT+ ( "." DIGIT+ )? ;
STRING         -> "\"" <any char except "\"">* "\"" ;
IDENTIFIER     -> ALPHA ( ALPHA | DIGIT )* ;
ALPHA          -> "a" ... "z" | "A" ... "Z" | "_" ;
DIGIT          -> "0" ... "9" ;
BOOL           -> true | false
```


## Expression
```
constant =    BOOL
            | NUMBER 
            | STRING

parentheses = "(" expression ")";



expression = constant | lambda | variable | function-application | parentheses | logical



variable = IDENTIFER

argument-variable = IDENTIFER
lambda-arguments = (argument-variable) (argument-variable)*;
lambda = "fun" lambda-arguments "->" expression;


application =   constant
                | variable
                | parentheses;


function-application = IDENTIFER (application) (application)*;








```

