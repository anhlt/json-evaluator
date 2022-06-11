CREATE TABLE users (
    phoneTypes LIST[STRING] VALUES("$.phoneNumbers[:1].type") ,
    userName STRING VALUES("$.firstName") DEFAULT "firstName", 
    amount INT VALUES("$.amount") DEFAULT 0;
);


SELECT phoneTypes, userName, amount * 0.5 AS reward from users
WHERE
    userName = "values"
    AND CONTAINS(phoneTypes, "iphone")
    AND amount > 100;