type Depth = Int
data Tree a = Void | Node a [Tree a] Depth
    deriving (Eq, Show, Ord)


--Test tree
leaf_a = (Node 1 [] 1)
leaf_b = (Node 2 [] 1)
leaf_c = (Node 3 [] 1)
leaf_d = (Node 4 [] 1)
leaf_e = (Node 5 [] 1)
leaf_f = (Node 6 [] 1)
leaf_g = (Node 7 [] 1)
branch_a = (Node 8 [leaf_a, leaf_b, leaf_c] 2)
branch_b = (Node 9 [leaf_d, leaf_e, leaf_f] 2)
root_a = (Node 10 [branch_a, branch_b, leaf_g] 3)

count :: Int -> [Int]
count 1 = [1]
count n = [n] ++ (count (n-1))

mass_update_depth :: [Tree a] -> [Tree a]
mass_update_depth ts = map (\x -> update_depth x) ts

update_depth :: Tree a -> Tree a
update_depth Void = Void
update_depth (Node l [] n) = (Node l [] 1)
update_depth (Node l ts n) = (Node l ts ((get_max_depth (mass_update_depth ts)) + 1))

get_depth :: Tree a -> Int
get_depth Void = 0
get_depth (Node l ts d) = d

get_max_depth :: [Tree a] -> Int
get_max_depth [] = 0
get_max_depth (t:ts) = max (get_depth t) (get_max_depth ts)

get_node :: Tree a -> Tree a
get_node Void = Void
get_node (Node l ts d) = (Node l [] 1)

combinations :: Ord a => [a] -> Int -> [[a]]
combinations xs n = [x | x <- mapM (const xs) [1..(max n (length xs))], head x < head (tail x) ]

combinations_up_to_r :: Ord a => [a] -> Int -> [[a]]
combinations_up_to_r xs n = concat (map (\x -> combinations xs x) (count n))


-- Bug: since combinations chooses the maximum between the actual number of children and n,
-- if a tree has a number of children smaller than n there will be multiple copies of the same subtree in the
-- output
get_children_combinations :: Ord a => Tree a -> Int -> [Tree a]
get_children_combinations Void _ = []
get_children_combinations (Node l [] n) _ = []
get_children_combinations (Node l ts n) r = map (\x -> (Node l x n)) (combinations_up_to_r ts r)

subtrees :: Ord a => Tree a -> Int -> Int -> [Tree a]
subtrees tree 1 _ = [get_node tree]
subtrees (Node l [] d) _ _ = [(Node l [] d)]
subtrees tree max_depth max_children = concat (concat (map (\y -> (map (\x -> subtrees y x max_children) (count max_depth))) (get_children_combinations tree max_children)))
