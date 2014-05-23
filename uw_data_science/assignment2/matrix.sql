
SELECT x.
FROM
(
     SELECT *
     FROM A as a
     CROSS JOIN B as b
) x;
