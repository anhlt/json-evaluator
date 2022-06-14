CREATE TABLE users (
    phoneTypes LIST [STRING] EXTRACT ("$.phoneNumbers[:1].type") ,
    userName STRING EXTRACT ("$.firstName") DEFAULT "firstName", 
    amount INT EXTRACT ("$.amount") DEFAULT 0;
    /* comment */
);


SELECT phoneTypes, userName, amount * 0.5 AS reward from users
WHERE
    userName = "values"
    AND CONTAINS(phoneTypes, "iphone")
    AND amount > 100;