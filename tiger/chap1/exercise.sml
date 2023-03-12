type key = string

datatype tree =
  LEAF
| TREE of tree * key * tree

val empty = LEAF

fun insert (key, LEAF) = TREE (LEAF, key, LEAF)
  | insert (key, TREE (l, k, r)) =
  if key < k then
    TREE (insert (key, l), k, r)
  else if key > k then
    TREE (l, k, insert (key, r))
  else
    TREE (l, key, r)

(* a. Implement a member function that returns true if the item is found, else ofalse. *)
fun member (_, LEAF) = false
  | member (item, TREE (l, k, r)) = item = k orelse member (item, if item > k then
  r
else
  l)

(* b. Extend the program to include not just membership, but the mapping of keys to bindings *)
structure ATree = struct
  datatype 'a atree =
    LEAF of 'a
  | TREE of 'a atree * key * 'a atree * 'a
end