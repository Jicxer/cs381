module HW2 where
import HW2types
import Data.List (sort)

-- The function takes in an Int
-- The base definition of a Leaf would have 0 nodes
-- If it is a node, sizeTree would call on left and right sub-trees and add 1 to their sizes
sizeTree :: Tree -> Int
sizeTree Leaf = 0
sizeTree (Node _ left right) = 1 + sizeTree left + sizeTree right

-- Function takes in an Int
-- The height of a leaf would return -1 since it is defined as such
-- If the input is a node, then height will be recrusviely called for the left and right
-- subtrees then add 1 to their maximum heights
height :: Tree -> Int
height Leaf = -1
height (Node _ left right) = 1 + max (height left) (height right)

-- Function takes in an Int and calculates the number of nodes in that tree
-- A leaf would have no nodes to be caluclated; return 0
-- treeSum is recursively applied to the left and right subtrees and returns the sum of all nodes in a tree
treeSum :: Tree -> Int
treeSum Leaf = 0
treeSum (Node x left right) = x + treeSum left + treeSum right

-- Function to overload the == operator
-- This would return True if the two trees contain the same value and False otherwise
-- Two leaves are equal
-- If both the nodes are equivalent if they have the same value and thus return true otherwise, return False
instance Eq Tree where
    Leaf == Leaf = True
    (Node x1 left1 right1) == (Node x2 left2 right2) = (x1 == x2) && (left1 == left2) && (right1 == right2)
    _ == _ = False

-- Takes two tree inputs and outputs a tree
-- If a tree is a leaf, it would return the other tree as there are no other values to merge
-- If both values have valid values, this creates a new node and with the sum of the values
-- Merge the subtrees from input
mergeTrees :: Tree -> Tree -> Tree
mergeTrees Leaf t = t
mergeTrees t Leaf = t
mergeTrees (Node x1 left1 right1) (Node x2 left2 right2) =
    Node (x1 + x2) (mergeTrees left1 left2) (mergeTrees right1 right2)

-- Takes a tree as an input and outputs either True or False based on whether the tree is BST
-- If the input is a leaf, it will be true
-- isBST otherwise checks whether each node satisfies the the BST condition
-- (is the value greater than equal to the min vlaue of its left subtree and less than or equal to the max of the right subtree)
isBST :: Tree -> Bool
isBST Leaf = True
isBST t = isBST' t minBound maxBound
  where
    isBST' Leaf _ _ = True
    isBST' (Node x left right) minVal maxVal =
      let leftOK = isBST' left minVal x
          rightOK = isBST' right x maxVal
      in x >= minVal && x <= maxVal && leftOK && rightOK

-- converBST turns a tree into a BST a helper function is created to sort the list
-- If the input is a leaf, it will be a leaf
-- Otherwise, we grab a list of all values in a tree and sort the list to create a new bst
-- This is done through the helper funciton buildBST
  -- Takes a sorted list of integers and placs those values into a tree
-- inOrderTraversal function does a traversal of a tree and returns all values of that tree
convertBST :: Tree -> Tree
convertBST Leaf = Leaf
convertBST t = buildBST (sort (inOrderTraversal t))
buildBST :: [Int] -> Tree
buildBST [] = Leaf
buildBST xs = Node mid (buildBST left) (buildBST right)
  where
    len = length xs
    midIndex = len `div` 2
    mid = xs !! midIndex
    left = take midIndex xs
    right = drop (midIndex + 1) xs
inOrderTraversal :: Tree -> [Int]
inOrderTraversal Leaf = []
inOrderTraversal (Node x left right) = inOrderTraversal left ++ [x] ++ inOrderTraversal right

-- Graphs --
-- numVE takes a graph as an input and returns the tuple that has 
-- the number of verticies and the number of edges
-- Duplicated edges are removed
numVE :: Graph -> (Int, Int)
numVE g = (numVertices, numEdges)
  where numVertices = length (vertices g)
        numEdges = length g

        vertices :: Graph -> [Vertex]
        vertices = foldr (\(u, v) vs -> u:v:vs) [] . removeDuplicates

        removeDuplicates :: Graph -> Graph
        removeDuplicates [] = []
        removeDuplicates ((u,v):es)
          | u == v    = removeDuplicates es
          | otherwise = (u,v) : removeDuplicates es

-- Takes a graph as an input
-- An empty graph would return an empty graph
-- If the curernt edge is a loop then the edge is skipped and call removeLoops on the edges
removeLoops :: Graph -> Graph
removeLoops [] = []
removeLoops ((u,v):es)
    | u == v    = removeLoops es
    | otherwise = (u,v) : removeLoops es

-- Input takes a vertex v and a graph G and returns a new graph with the vertex is removed
-- An empty graph will return an empty list
-- check if the endpoints x or y are equal to v. 
    -- If so, skip and move to another edge
    -- Else, include into the new graph
removeVertex :: Vertex -> Graph -> Graph
removeVertex _ [] = []
removeVertex v ((x,y):es)
    | v == x || v == y = removeVertex v es
    | otherwise = (x,y) : removeVertex v es
