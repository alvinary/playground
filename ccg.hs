type Name = String

data Side = LeftDirection | RightDirection
	deriving (Eq, Show)

data Type = Void | Atomic Name | Functor Type Side Type
	deriving (Eq, Show)

data Expression = Empty | Simple Name Type | Compound Expression Expression Type
	deriving (Eq, Show)

number = (Atomic "n")
binary_operation = (Functor number LeftDirection half_binary)
half_binary = (Functor number RightDirection number)
unary_operation = (Functor number LeftDirection number)

pl = (Simple "+" binary_operation)
mi = (Simple "-" binary_operation)
on = (Simple "O" number)
sigu = (Simple "S" unary_operation)

atoms = [pl, mi, on, sigu]

get_type :: Expression -> Type
get_type Empty = Void
get_type (Simple n t) = t
get_type (Compound e f t) = t

get_output :: Type -> Type
get_output Void = Void
get_output (Atomic n) = (Atomic n)
get_output (Functor t d s) = s

get_input_type :: Type -> Type
get_input_type Void = Void
get_input_type (Atomic n) = (Atomic n)
get_input_type (Functor t f s) = t

get_direction :: Type -> Side
get_direction (Functor t1 s t2) = s
get_direction _ = error "Atomic or void type does not have a direction."

is_functor :: Type -> Bool
is_functor (Functor t d s) = True
is_functor _ = False

composable :: Expression -> Expression -> Bool
composable Empty _ = False
composable _ Empty = False
composable e1 e2 | is_functor (get_type e1) = ((get_input_type (get_type e1)) == (get_type e2)) 
                 | otherwise = False

compose :: Expression -> Expression -> Expression
compose e1 e2 | get_direction (get_type e1) == LeftDirection = (Compound e1 e2 (get_output (get_type e1)))
              | get_direction (get_type e1) == RightDirection = (Compound e2 e1 (get_output (get_type e1))) 
compose _ _ = error "Cannot compose void type or atomic type"

apply :: Expression -> Expression -> Expression
apply e f | composable e f = compose e f
          | otherwise = Empty
          
mass_apply :: Expression -> [Expression] -> [Expression]
mass_apply e [] = []
mass_apply e (x:xs) = ((apply e x):(mass_apply e xs))

generate :: Int -> Expression -> [Expression]
generate 1 basis = filter (\x -> not (x == Empty)) basis
generate n basis = generate (n - 1) basis ++ (filter (\x -> not (x == Empty)) (concat (map (\x -> (mass_apply x (generate (n - 1) basis))) (generate (n-1) basis))))

string_rep :: Expression -> String
string_rep Empty = "0"
string_rep (Simple n t) = " " ++ n ++ " "
string_rep (Compound e1 e2 t) = (string_rep e1) ++ (string_rep e2)

display_gen :: Int -> [String]
display_gen n = map (\x -> string_rep x) (generate n)

display_num :: Int -> [String]
display_num n = map (\x -> string_rep x) (filter (\y -> (get_type y) == number) (generate n))
