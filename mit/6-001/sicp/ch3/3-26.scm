###3-26###
PROMPT:
------------------------------------------------------------
Exercise 3.26.  To search a table as implemented above, one needs to scan through the list of records. This is basically the unordered list representation of section 2.3.3. For large tables, it may be more efficient to structure the table in a different manner. Describe a table implementation where the (key, value) records are organized using a binary tree, assuming that keys can be ordered in some way (e.g., numerically or alphabetically). (Compare exercise 2.66 of chapter 2.) 
------------------------------------------------------------

An implementation utilizing a binary tree can be achieved by restructuring the way that tables store (key, value) pairs. For instance, a 1-dimensional table lookup would use a different definition of assoc. This function would take a key and a table, just as before. Through data abstraction, each node in the tree would have selectors for value, left, and right. The assoc function would check the value of the node. Of course, in a 1-dimensional table, the value of a node can only be a record, which is a (key, value) pair. Therefore, assoc checks the car of value, and checks the given key with the key in the current node. If equal, the cdr of the value of the node is returned. Otherwise, assoc is called on the left and right selectors, which are pointers to the child nodes of the current node. Thus, we recursively check the entire tree for matches. If none are found, then false is returned.

A n-dimensional table complicates things. Now, we need a selector to operate on the value node, so that we have an abstract (get-key) function. This way, the underlying structure doesn't matter, since it could be either another table, or a record. We can have a conditional that checks for the type of structure stored in value, and if a record, return the record, otherwise call assoc on the level down with the next key in our list of given keys.

Assignment is more tricky. Given an arbitrary list of keys, we check the length. If length equals 2, then the first value is a key and the second is the value and we just make a record, and use a function to attach the record in the most appropriate place. This function will check the key of the value selector, and if value is equal, set the value to the new record. Otherwise, it checks whether the key is less than or greater than the key stored under value, and calls itself recursively on the appropriate child node. If the list of keys is greater than length 2, we search for a matching key exactly the same way, however when we find an equal key or a dead-end, we set value to a new table stored under the current key, and then call the attach procedure on the cdr of our keys list. Note here that a dead end is when a node does not equal the given key, but a child node does not exist in the correct direction (< or >).





