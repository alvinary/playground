import Control.Monad

type Name = String

data Direction = L | R | E
    deriving (Eq, Show, Ord)

data Modus = Comp | Adj | Neuter
    deriving (Eq, Ord, Show)

data Type = Empty | Atomic Name | Functor Type Direction Modus Type
    deriving (Eq, Show, Ord)

data Expression = Void | Simple Type Name | Compound Type Expression Expression Direction
    deriving (Eq, Show, Ord)

data Symbol = Constant Name
    deriving (Eq, Show, Ord)

data Atom = Predicate Symbol [Symbol]
    deriving (Eq, Show, Ord)

data Rule = Head Atom [Atom]
    deriving (Eq, Show, Ord)

data Fact = Assertion Atom
    deriving (Eq, Show, Ord)

data DRS = Dummy | Constraints [Symbol] [Fact] Symbol [(Symbol, Symbol)] Symbol | Compose DRS DRS Modus Direction
    deriving (Eq, Show, Ord)

type SemanticExpression = (Expression, DRS)

type Interpretation = [(Expression, DRS)]

----Functions for generating expressions

--Get first element of a tuple
first :: (x, y) -> x
first (a, b) = a

--Get second element of a tuple
second :: (x, y) -> y
second (a, b) = b

--Get the type of an expression
get_type Void = Empty
get_type (Simple t n) = t
get_type (Compound t e1 e2 s) = t

--Get input type of a functor type
get_input (Functor t1 d m t2) = t1
get_input _ = error "Only functor types have input types."

--Get output type of a functor type
get_output (Functor t1 d m t2) = t2
get_output _ = error "Only functor types have output types."

get_modus (Functor t1 d m t2) = m
get_modus (Atomic t) = error "Atomic types do not have a modus."
get_modus Empty = error "Empty types do not have a modus."

--Get the direction of a functor type (which determines whether expressions with that
--type apply left or apply right)
get_side (Functor t1 d m t2) = d
get_side _ = error "Only functor types have directions."

--Determine if a given type is the input type of a functor
match_types :: Type -> Type -> Bool
match_types (Functor t1 d m t2) t3 = (t1 == t3)
match_types _ _ = False

--Determine if the types of two expressions match
match_by_type :: Expression -> Expression -> Bool
match_by_type e1 e2 = match_types (get_type e1) (get_type e2)

--Form a compound expression
compose :: Expression -> Expression -> Expression
compose e1 e2 | get_side (get_type e1) == L = (Compound (get_output (get_type e1)) e1 e2 L)
              | get_side (get_type e1) == R = (Compound (get_output (get_type e1)) e2 e1 R)
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
show_expression (Compound t e1 e2 s) = "(" ++ (show_expression e1) ++ " " ++ (show_expression e2) ++ ")"

----Functions for writing expressions to ASP syntax

--Return all variables in a DRS

--Return all distinct elements in a list
distinct :: (Eq a) => [a] -> [a]
distinct [] = []
distinct (x:xs) | elem x xs = distinct xs
                | otherwise = (x:(distinct xs))

--
enumerate :: Int -> [a] -> [(a, Int)]
enumerate _ [] = []
enumerate n (x:xs) = ((x, n + (length (x:xs))) : (enumerate n xs))

--Given a Semantic Expression, associate all variables in the DRS it contains to a new unique name,
--and return a list of tuples of the form (variable_name, new_variable_name).
get_bindings :: Int -> DRS -> [(Symbol, Symbol)]
get_bindings n (Constraints v f s a h) = map (\x -> (first x, (Constant ("c" ++ show (second x))))) (enumerate n (distinct (s:v)))
get_bindings n (Compose d1 d2 mod dir) = (get_bindings n d1) ++ (get_bindings (n + m) d2) where m = length (get_bindings n d1)

--Get the variable symbols in a non-composite DRS
get_variables :: DRS -> [Symbol]
get_variables (Constraints v f s a h) = v
get_variables _ = error "Cannot get variables from DRS composition or dummy DRS."

--Get the facts in a non-composite DRS list of constraints
get_constraints :: DRS -> [Fact]
get_constraints (Constraints v f s a h) = f
get_constraints _ = error "Cannot get constraints from DRS composition or dummy DRS."

get_drs_variable :: DRS -> Symbol
get_drs_variable (Constraints v f s a h) = s
get_drs_variable _ = error "Cannot get DRS variable from a dummy DRS or composition DRS."

get_drs_arguments :: DRS -> [(Symbol, Symbol)]
get_drs_arguments (Constraints v f s a h) = a
get_drs_arguments _ = error "Cannot get arguments from dummy DRS or composition DRS."

get_head :: DRS -> Symbol
get_head (Constraints v f s a h) = h
get_head _ = error "Cannot get head from dummy DRS or composition DRS."

--Map a symbol to another symbol
map_symbol :: Symbol -> [(Symbol, Symbol)] -> Symbol
map_symbol s [] = s
map_symbol s (p:ps) | s == (first p) = second p
                    | s /= (first p) = map_symbol s ps

--Rename all symbols present in the bindings provided to their bound names (in a fact)
rename_fact :: [(Symbol, Symbol)] -> Fact -> Fact
rename_fact bindings (Assertion (Predicate predicate arguments)) = (Assertion (Predicate predicate renamed_arguments))
    where
        renamed_arguments = map (\x -> map_symbol x bindings) arguments

--Rename all symbols present in a list of facts to their bound names
rename :: [(Symbol, Symbol)] -> [Fact] -> [Fact]
rename bindings facts = map (\f -> rename_fact bindings f) facts

--Rename all symbols in a list of symbol pairs to their bound names
rename_arguments :: [(Symbol, Symbol)] -> [(Symbol, Symbol)] -> [(Symbol, Symbol)]
rename_arguments bindings arguments = map (\x -> (first x, map_symbol (second x) bindings)) arguments

--Given a list of bindings, a list of arguments, and the name of a head...
--Remember to have bindings for both branches
argument_facts :: [(Symbol, Symbol)] -> [(Symbol, Symbol)] -> Symbol -> [Fact]
argument_facts b a h = rename b [unification_declaration]
    where
        unification_declaration = (Assertion (Predicate (Constant "unify") [(second current_argument), h]))
        current_argument = head a

new_head :: DRS -> DRS -> Modus -> Symbol
new_head d1 d2 Comp = get_head d1
new_head d1 d2 Adj = get_head d2
new_head _ _ Neuter = error "Heads for the composition of DRS can only be formed from neuter modus."

get_fresh_symbol :: [Symbol] -> Symbol
get_fresh_symbol xs = (Constant ("f" ++ show ((length xs) + 3)))


--Must refactor this so that
--composition facts = [drs_declaration, composition_declaration, suitable_declaration(f1, f2, f3, modus, side)]
composition_facts :: Symbol -> Symbol -> Symbol -> Modus -> Direction -> [Fact]
composition_facts f1 f2 f3 Comp L = [drs_declaration_fact, composition_declaration, complementation_declaration]
    where
        composition_declaration = (Assertion (Predicate (Constant "compose") [f1, f2, f3]))
        drs_declaration_fact = (Assertion (Predicate (Constant "drs") [f3]))
        complementation_declaration = (Assertion (Predicate (Constant "complement") [f1, f2]))
composition_facts f1 f2 f3 Adj L = [drs_declaration_fact, composition_declaration, adjunction_declaration]
    where
        composition_declaration = (Assertion (Predicate (Constant "compose") [f1, f2, f3]))
        drs_declaration_fact = (Assertion (Predicate (Constant "drs") [f3]))
        adjunction_declaration = (Assertion (Predicate (Constant "adjunct") [f1, f2]))
composition_facts f1 f2 f3 Comp R = [drs_declaration_fact, composition_declaration, complementation_declaration]
    where
        composition_declaration = (Assertion (Predicate (Constant "compose") [f2, f1, f3]))
        drs_declaration_fact = (Assertion (Predicate (Constant "drs") [f3]))
        complementation_declaration = (Assertion (Predicate (Constant "complement") [f2, f1]))
composition_facts f1 f2 f3 Adj R = [drs_declaration_fact, composition_declaration, adjunction_declaration]
    where
        composition_declaration = (Assertion (Predicate (Constant "compose") [f2, f1, f3]))
        drs_declaration_fact = (Assertion (Predicate (Constant "drs") [f3]))
        adjunction_declaration = (Assertion (Predicate (Constant "adjunct") [f2, f1]))

--Compose a DRS
compose_drs :: DRS -> DRS
compose_drs (Constraints v f s a h) = (Constraints v f s a h)
compose_drs (Compose d1 d2 m s) = (Constraints (map (\x -> second x) (b1 ++ b2)) ((rename b1 c1) ++ (rename b2 c2) ++ (composition_facts (map_symbol f1 b1) (map_symbol f2 b2) f3 m s)) f3 (tail (rename_arguments b1 as)) nh)
    where
        b1 = get_bindings 0 (compose_drs d1)
        b2 = get_bindings (length b1) (compose_drs d2)
        c1 = get_constraints (compose_drs d1)
        c2 = get_constraints (compose_drs d2)
        f1 = get_drs_variable d1
        f2 = get_drs_variable d2
        f3 = get_fresh_symbol ((get_variables d1) ++ (get_variables d2))
        as = get_drs_arguments d1
        af = argument_facts (b1 ++ b2) as (get_head d2)
        nh = map_symbol (new_head d1 d2 m) (b1 ++ b2)

--Map an expression to a semantic expression
endow :: Interpretation -> Expression -> SemanticExpression
endow [] (Simple t n) = error "The simple expression provided as argument has no interpretation."
endow (i:is) (Simple t n) | (first i) == (Simple t n) = i
                          | otherwise = endow is (Simple t n)
endow i (Compound t e1 e2 s) | (s == L) = ((Compound t e1 e2 L), (compose_drs (Compose (compose_drs (second (endow i e1))) (compose_drs (second (endow i e2))) (get_modus (get_type e1)) (get_side (get_type e1)))))
endow i (Compound t e1 e2 s) | (s == R) = ((Compound t e1 e2 R), (compose_drs (Compose (compose_drs (second (endow i e1))) (compose_drs (second (endow i e2))) (get_modus (get_type e2)) (get_side (get_type e2)))))


--Write rules and facts
write_symbol (Constant n) = n

write_arguments :: [Symbol] -> String
write_arguments [] = ""
write_arguments (a:[]) = write_symbol a ++ ")"
write_arguments (a:as) = write_symbol a ++ ", " ++ write_arguments as

write_atom (Predicate s ss) = write_symbol s ++ "(" ++ write_arguments ss

write_fact (Assertion p) = write_atom p ++ "."

write_facts :: [Fact] -> String
write_facts [] = ""
write_facts (f:fs) = write_fact f ++ "\n" ++ write_facts fs

write_variables :: [Symbol] -> String
write_variables [] = ""
write_variables (s:[]) = write_symbol s
write_variables (s:ss) = write_symbol s ++ ", " ++ write_variables ss

write_drs :: DRS -> String
write_drs (Constraints v f s a h) = "%% Variables: " ++ write_variables (s:v) ++ "\n%%Constraints: \n" ++ write_facts f ++ "\n"
write_drs (Compose d1 d2 m s) = error "Cannot write unevaluated DRS composition."

write_semexp :: SemanticExpression -> String
write_semexp (e, s) = show_expression e ++ ", " ++ write_drs s

write_example :: SemanticExpression -> String
write_example (e, s) = "<example> \n\n" ++ "%% Axioms and WK: \n" ++ axioms ++ background_knowledge ++ "%% " ++ show_expression e ++ "\n" ++ write_drs s ++ "</example> \n\n"

--Example

show_type basis t d = map (\x -> show_expression x) (generate_type basis t d)

int_type = (Atomic "N")
int_on_int_type = (Functor (Atomic "N") L Comp (Atomic "N"))
binary_int_type = (Functor (Atomic "N") R Comp int_on_int_type)

noun = (Atomic "n")
sentence = (Atomic "s")
adjective = (Functor noun R Adj noun)
intransitive = (Functor noun R Comp sentence)
transitive = (Functor noun L Comp intransitive)
relative = (Functor intransitive R Comp adjective)

man = (Simple noun "man")
building = (Simple noun "building")
woman = (Simple noun "woman")
saw = (Simple transitive "saw")
large = (Simple adjective "large")
tall = (Simple adjective "tall")
walked = (Simple intransitive "walked")
that = (Simple relative "that")
the = (Simple adjective "the")

read_symbols :: [String] -> [Symbol]
read_symbols ss = map (\x -> (Constant x)) ss

read_fact :: [String] -> Fact
read_fact [] = error "Empty facts are not allowed"
read_fact xs = (Assertion (Predicate (Constant (head xs)) (map (\x -> (Constant x)) (tail xs))))

read_facts :: [[String]] -> [Fact]
read_facts xs = map (\x -> read_fact x) xs

the_facts = []
the_semantics = []

read_drs :: ([String], [[String]], String, [(String, String)], String) -> DRS
read_drs (constants, facts, drs_symbol, arguments, head) = (Constraints rc rf rd ra rh)
    where
        rc = read_symbols constants
        rf = read_facts facts
        rd = (Constant drs_symbol)
        ra = map (\x -> ((Constant (first x)), (Constant (second x)))) arguments
        rh = (Constant head)

man_drs = read_drs ( ["v1"]
               , [ ["true", "man", "v1"]
                 , ["head", "dman", "v1"]
                 , ["drs", "dman"]]
               , "dman"
               , []
               , "v1" )

woman_drs = read_drs ( ["v1"]
               , [ ["true", "woman", "v1"]
                 , ["head", "dwoman", "v1"]
                 , ["drs", "dwoman"]]
               , "dwoman"
               , []
               , "v1" )

building_drs = read_drs ( ["v1"]
               , [ ["true", "building", "v1"]
                 , ["head", "dbuilding", "v1"]
                 , ["drs", "dbuilding"]]
               , "dbuilding"
               , []
               , "v1" )

tall_drs = read_drs ( ["v1", "p1"]
               , [ ["true", "tallness", "p1"]
                 , ["head", "dtall", "p1"]
                 , ["true", "tall", "v1"]
                 , ["require", "personal", "v1"]]
               , "dtall"
               , [("tall", "v1")]
               , "p1" )

saw_drs = read_drs ( ["v1", "v2", "e1"]
               , [ ["true", "sight", "e1"]
                 , ["head", "dsee", "e1"]
                 , ["true", "seen", "v1"]
                 , ["true", "seer", "v2"]
                 , ["require", "visible", "v1"]
                 , ["require", "can_see", "v2"]]
               , "dsee"
               , [("seen", "v1"), ("seer", "v2")]
               , "e1" )

that_drs = read_drs ( ["v1"]
               , [ []
                 , []
                 , []]
               , ""
               , [("", "")]
               , "" )

the_drs = read_drs ( ["v1", "p1"]
               , [ ["true", "determinate", "v1"]
                 , ["true", "definite", "p1"]
                 , ["head", "dthe", "p1"]
                 , ["drs", "dthe"]
                 , ["reject_backwards", "determinate", "v1"]]
               , "dthe"
               , [("determinate", "v1")]
               , "p1" )

-- Axioms for DRS behavior

-- One solution is to make unification an order relation instead of an equivalence relation,
-- so that you can reject a property of a complement, but have the result of the composition
-- attributed the rejected property anyway (as in the case of determiners, colors, and the like).

-- Another solution is to add more relations and axioms
-- Unify, unify_forward, reject, reject_backwards, reject_forward

axioms = concat 
        [ "unify(A, B) :- unify(B, A). \n"
        , "unify(A, C) :- unify(A, B), unify(B, C). \n"
        , "true(P, B, C) :- unify(A, B), true(P, A, C). \n"
        , "true(P, B) :- true(P, A), unify(A, B). \n"
        , "false(P, B, C) :- unify(A, B), false(P, A, C). \n"
        , "false(P, B) :- false(P, A), unify(A, B). \n"
        , ":- require(P, A), not true(P, A). \n"
        , ":- reject(P, A), true(P, A). \n"
        , ":- require(P, A, B), not true(P, A, B). \n"
        , ":- reject(P, A, B), true(P, A, B). \n"
        , ":- false(P, A, B), true(P, A, B). \n"
        , ":- false(P, A), true(P, A). \n"
        , "unify(C1, C2) :- compose(D, E, F), head(D, C1), head(F, C2), complement(D, E). \n"
        , "unify(C1, C2) :- compose(D, E, F), head(E, C1), head(F, C2), adjunct(D, E). \n" ]

background_knowledge = concat
                      [ "true(can_see, X) :- true(personal, X). \n"
                      , "true(visible, X) :- true(material, X). \n"
                      , "true(material, X) :- true(place, X). \n"
                      , "true(material, X) :- true(personal, X). \n"
                      , "true(personal, X) :- true(man, X). \n"
                      , "true(personal, X) :- true(woman, X). \n"
                      , "true(can_see, X) :- true(personal, X). \n"
                      , "true(place, X) :- true(buidling, X). \n"]

lexicon = [man, woman, building, the, tall, saw]
lexicon_interpretation = [ (man, man_drs)
                         , (woman, woman_drs)
                         , (saw, saw_drs)
                         , (building, building_drs)
                         , (the, the_drs)
                         , (saw, saw_drs)
                         , (tall, tall_drs)]


zero_lex = (Simple int_type "O")
succ_lex = (Simple int_on_int_type "S")
plus_lex = (Simple binary_int_type "+")

sample_basis = [zero_lex, succ_lex, plus_lex]

nums = show_type sample_basis int_type 4

rocha = generate_type lexicon sentence 4

ropa = map (\x -> endow lexicon_interpretation x) rocha

arropados = map (\x -> (first x, compose_drs (second x))) ropa

arrorros = map (\x -> write_example x) arropados

main = do 
        forM_ arrorros putStr
        forM_ nums $ \x -> putStr x

