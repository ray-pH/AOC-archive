:: a.pro
Uses dynamic predicate to store visited square,
which is weird because now predicates like `store_beams` have "side effects"
it's hard because there isn't really a nice hashmap like object in prolog,
and people said that the closest thing is dynamic predicate.

:: a1.pro
Tried to change the way to store visited square, to make it more "idiomatic?".
Tried just use a list, and boy was it a bad idea,
checking `member(X, List)` is very slow

:: a2.pro
Uses Association List to store the visited square,
the code is more "idiomatic",
the caveat is that Association List uses AVL trees which have O(log(n)) complexity for accessing and storing data,
which is not ideal

:: b.pro
Uses Association List, and just brute force
