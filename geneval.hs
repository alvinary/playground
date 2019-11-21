type Name = String

data Direction = L | R | E
    deriving (Eq, Show, Ord)

data Type = Empty | Atomic Name | Functor Type Direction Type
    deriving (Eq, Show, Ord)

data Expression = Void | Simple Type Name | Compound Type Expression Expression
    deriving (Eq, Show, Ord)

--Get first element of a tuple
first :: (x, y) -> x
first (a, b) = a

--Get second element of a tuple
second :: (x, y) -> y
second (a, b) = b

--Get the type of an expression
get_type Void = Empty
get_type (Simple t n) = t
get_type (Compound t e1 e2) = t

--Get input type of a functor type
get_input (Functor t1 d t2) = t1
get_input _ = error "Only functor types have input types."

--Get output type of a functor type
get_output (Functor t1 d t2) = t2
get_output _ = error "Only functor types have output types."

--Get the direction of a functor type (which determines whether expressions with that
--type apply left or apply right)
get_side (Functor t1 d t2) = d
get_side _ = error "Only functor types have directions."

--Determine if a given type is the input type of a functor
match_types :: Type -> Type -> Bool
match_types (Functor t1 d t2) t3 = (t1 == t3)
match_types _ _ = False

--Determine if the types of two expressions match
match_by_type :: Expression -> Expression -> Bool
match_by_type e1 e2 = match_types (get_type e1) (get_type e2)

--Form a compound expression
compose :: Expression -> Expression -> Expression
compose e1 e2 | get_side (get_type e1) == L = (Compound (get_output (get_type e1)) e1 e2)
              | get_side (get_type e1) == R = (Compound (get_output (get_type e1)) e2 e1)
              | get_side (get_type e1) == E = error "A functor type is not supposed to have the empty direction."
compose _ _ = error "Cannot compose the expressions provided as arguments"

--Compose two expressions if their types match, evaluate to void otherwise.
apply_expressions :: Expression -> Expression -> Expression
apply_expressions e1 e2 | match_by_type e1 e2 = compose e1 e2
                        | otherwise = Void

--Filter all void expressions
filter_void :: [Expression] -> [Expression]
filter_void es = filter (\x -> x /= Void) es

--Compute the cartesian product of a list with itself
self_product :: [a] -> [(a, a)]
self_product xs = (concat (map (\x -> (map (\y -> (x, y)) xs)) xs))

--Apply all expressions in a list to each member of the list
self_compose :: [Expression] -> [Expression]
self_compose es = filter_void ((map (\x -> apply_expressions (first x) (second x))) (self_product es))

--Generate all expressions up to depth d
generate_all :: [Expression] -> Int -> [Expression]
generate_all basis 1 = basis
generate_all basis n = concat [(self_compose (generate_all basis (n - 1))), (generate_all basis (n - 1))]

--Generate all expressions of a given type up to depth d
generate_type :: [Expression] -> Type -> Int -> [Expression]
generate_type basis t d = filter (\x -> (get_type x) == t) (generate_all basis d) 

--Return a string representing an expression
show_expression Void = "0"
show_expression (Simple t n) = n
show_expression (Compound t e1 e2) = "(" ++ (show_expression e1) ++ " " ++ (show_expression e2) ++ ")"

--Example

show_type basis t d = map (\x -> show_expression x) (generate_type basis t d)

int_type = (Atomic "N")
int_on_int_type = (Functor (Atomic "N") L (Atomic "N"))
binary_int_type = (Functor (Atomic "N") R int_on_int_type)

zero_lex = (Simple int_type "O")
succ_lex = (Simple int_on_int_type "S")
plus_lex = (Simple binary_int_type "+")

sample_basis = [zero_lex, succ_lex, plus_lex]
