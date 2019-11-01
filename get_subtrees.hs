type Depth = Int
data Tree a = Void | Node a [Tree a] Depth
    deriving (Eq, Show)

count :: Int -> [Int]
count 1 = [1]
count n = [n] ++ (count n-1)

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

get_children_combinations :: Tree a -> Int -> [Tree a]
get_children_combinations Void _ = []
get_children_combinations (Node l [] n) _ = []
get_children_combinations (Node l ts n) r = combinations ts r

-- Donde pingo meto las combinaciones de children?

subtrees :: Tree a -> Int -> Int -> [Tree a]
subtrees tree 1 _ = get_node tree
subtrees tree max_depth max_children = concat (map (\x -> subtrees tree x max_children) (count max_depth)
