data TestData = One | Two | Three deriving (Show, Read, Eq)

prueba 1 = One
prueba 2 = Two
prueba 3 = Three

data Tree = Leaf | Node Int Tree Tree deriving (Show, Read)

treeData = Node 3 Leaf Leaf

--TREE DEPTH



treeDepth :: Tree -> Int 
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree rightSubtree) =
    1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)




