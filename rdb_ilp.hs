import Control.Monad

type Name = String

type Composer = (EType -> Type)

type Checker = (Type -> Bool)

data Direction = L | R | E
    deriving (Eq, Show, Ord)

data Type = Empty
          | Atomic Name
          | Functor Type Direction Checker Composer Type
          | Pair Type Type
          | UnionType [Type]
          | Relation Type
    deriving (Eq, Show, Ord)

data Expression = Void 
                | Simple Type Name
                | Compound Type Expression Expression
    deriving (Eq, Show, Ord)

--Get first element of a tuple
first :: (x, y) -> x
first (a, b) = a

--Get second element of a tuple
second :: (x, y) -> y
second (a, b) = b

--Remove duplicates from a list
remove_duplicates :: (Eq a) => [a] -> [a]
remove_duplicates [] = []
remove_duplicates (x:xs) | elem x xs = remove_duplicates xs
                         | otherwise = (x:remove_duplicates xs)

-- Check if all distinct elements in xs are in ys
is_sublist :: (Eq a) => [a] -> [a] -> Bool
is_sublist [] ys = True
is_sublist (x:xs) ys = (elem x ys) && (is_sublist xs ys)

--Get the type of an expression
get_type Void = Empty
get_type (Simple t n) = t
get_type (Compound t e1 e2) = t

--Get input type of a functor type
get_input (Functor t1 d c co t2) = t1
get_input _ = error "Only functor types have input types."

--Get output type of a functor type
get_output (Functor t1 d c co t2) = t2
get_output _ = error "Only functor types have output types."

--Get the direction of a functor type (which determines whether expressions with that
--type apply left or apply right)
get_side (Functor t1 d c co t2) = d
get_side _ = error "Only functor types have directions."

--Get the input checker function of a functor type
get_side (Functor t1 d c co t2) = c
get_side _ = error "Only functor types have input checkers."

--Get the type composer of a functor type
get_side (Functor t1 d c co t2) = co
get_side _ = error "Only functor types have type composers"


--Determine if a given type is accepted by the input checker of a functor type
match_types :: Type -> Type -> Bool
match_types (Functor t1 d c co t2) t3 = c t3

--Determine if the types of two expressions match
match_by_type :: Expression -> Expression -> Bool
match_by_type e1 e2 = match_types (get_type e1) (get_type e2)

--Form a compound expression
compose :: Expression -> Expression -> Expression
compose e1 e2 | get_side (get_type e1) == L = (Compound (get_output (get_type_composer e1)) e1 e2)
              | get_side (get_type e1) == R = (Compound (get_output (get_type_composer e1)) e2 e1)
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
show_expression (Compound t e1 e2) = "(" ++ (show_expression e1) ++ " " ++ (show_expression e2) ++ ")" ++ "\n"


-- Upon receiving the first argument, the type composer assembles a type checker (lambda blah)

template_checker :: Type -> (Type -> Type -> Bool) -> Checker
template_checker t ce = (\x ->  ce t x)

-- When defining an op(eration), take care that the first argument is the "supply" type
-- and the second argument is the "variable" type
template_composer :: Type -> (Type -> Type -> Type) -> Composer
template_composer t op = (\x -> op t x)

make_union_type :: Type -> Type -> Type
make_union_type (UnionType t) (UnionType s) = (UnionType (remove_duplicates (t ++ s)))
make_union_type (UnionType t) (Relation s) = (UnionType (remove_duplicates (t ++ [s])))
make_union_type (Relation t) (UnionType s) = (UnionType (remove_duplicates (s ++ [t])))
make_union_type (Relation t) (Relation s) = (UnionType ([t] ++ [s]))
make_union_type _ _ = error "I cannot make a union type out of the types provided. "


-- How should union types be managed? 
make_composition_type :: Type -> Type -> Type
make_composition_type (Relation (Pair t1 t2)) (Relation (Pair t2 t3)) = (Relation (Pair t1 t3))
make_composition_type (Relation (Pair t1 t2)) (Relation (Pair t2 t3)) = (Relation (Pair t1 t3))

-- How should the inversion of union types be managed?

-- I think there's a problem here with pattern matching. What happens if t in the second equation is a uniontype?
-- Maybe that equation will be evaluated after the first equation. But if it doesn't, elem t ts
-- is a type error.

demand_included :: Type -> Type -> Bool -- (intersection)
demand_included (Relation (UnionType ts)) (Relation (UnionType us)) = is_sublist ts us
demand_included (Relation t) (Relation (UnionType ts)) = elem t ts
demand_included t1 t2 = t1 == t2
demand_included _ _ = False

-- This type checker accepts relations.
demand_relation :: Type -> Bool -- (virtually all)
demand_relation (Relation t) = True
demand_relation _ = False

-- beam_filter (keep the N expressions that best fit the data)

--Example

show_type basis t d = map (\x -> show_expression x) (generate_type basis t d)

id_type = (Atomic "Id")
temp_type = (Atomic "Temperature")
wind_type = (Atomic "Wind speed")
moist_type = (Atomic "Moisture")
plancton_type = (Atomic "Plancton")
day_type = (Atomic "Day_Moment")
depth_type = (Atomic "Depth")

int_on_int_type = (Functor (Atomic "R") R (Atomic "R"))
binary_int_type = (Functor (Atomic "R") L int_on_int_type)

zero_lex = (Simple int_type "A")
one_lex = (Simple int_type "B")
two_lex = (Simple int_type "C")
three_lex = (Simple int_type "D")
four_lex = (Simple int_type "E")
five_lex = (Simple int_type "F")
six_lex = (Simple int_type "G")
ten_lex = (Simple int_type "H")
eleven_lex = (Simple int_type "I")
twelve_lex = (Simple int_type "J")
thirteen_lex = (Simple int_type "K")
fourteen_lex = (Simple int_type "L")

-- Need support for unary relations

closure_lex = (Simple int_on_int_type "*") -- Preserves type
inversion_lex = (Simple int_on_int_type "^") -- Should invert tuple types
diff_lex = (Simple binary_int_type "-") -- both relations should have the same type (or the type of the second argument should be included in the first)
uni_lex = (Simple binary_int_type "|") -- does this create union types? Does that make sense? Nop nop
inter_lex = (Simple binary_int_type "&") -- this one absolutely requires its inputs to belong to the same type - no, wait, it works like relation difference
comp_lex = (Simple binary_int_type ">") -- (A, B) -> (B, C) -> (A, C). But what if we have union types?

-- Should I generalize operations?
-- Each operation would have a type checker (T -> bool), or (T -> T -> bool), an application function (its semantics),
-- and... sth else?

sample_basis = [zero_lex,  one_lex, two_lex, three_lex, four_lex, five_lex, six_lex, ten_lex, eleven_lex, twelve_lex, thirteen_lex, fourteen_lex, closure_lex, inversion_lex, diff_lex, uni_lex, inter_lex, comp_lex]

main = do
        forM_ (show_type sample_basis (Atomic "R") 5) putStr
