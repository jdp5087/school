SELECT count(*)
FROM (
     SELECT docid
     FROM frequency as f
     GROUP BY docid
     HAVING f.term = "transactions"
     OR f.term = "world"
);
