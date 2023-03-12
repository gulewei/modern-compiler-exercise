use "exercise.sml";

val bst = TREE (TREE (LEAF, "4", LEAF), "5", TREE (LEAF, "6", LEAF))

val bst2 = insert ("3", insert ("2", bst))

val test1 = member ("3", bst2)

val test2 = member ("7", bst2)