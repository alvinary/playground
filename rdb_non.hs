-- Todo - higher order queries
-- Todo - user functions
-- Todo - max
-- Todo - turn csv with tuples of arbitrary arity to binary relations
-- -- (for row in csv, row_id = new_id, for column in row, (new_id, column.element):column_name_relation_name)

-- Todo - boolean functions
-- Todo - such X (cond X)

-- Todo - program shouldn't crash on a parse error on the repl.
-- parse should return maybe expression, and when nothing is returned,
-- the repl should tell you "nope nope, this is not a legal expression",
-- and prompt you to introduce a new input

-- Todo - RDB should be able to read programs.

-- Todo - (dump) statement

import Control.Monad
import System.IO

import qualified Data.Set as Set
import qualified Data.Map as Map

data Constant = Individual String | Floating Float | Integer Int | Boolean Bool
    deriving (Eq, Ord, Show)

type Pair = (Constant, Constant)

type Relation = (Set.Set Pair)

data ElementType = Re | Co | In | Pa | Op

data Expression = Atom String | Sequence [Expression] | Operator String | SquareSequence [Expression]
    deriving (Eq, Ord, Show)

type State = Map.Map Expression Relation

type Definition = Map.Map Expression Expression


-- Assertion dataelement bool

first :: (a, b) -> a
first (x, y) = x

second :: (a, b) -> b
second (x, y) = y

relation_intersection :: Relation -> Relation -> Relation
relation_intersection rel_a rel_b = Set.intersection rel_a rel_b

relation_union :: Relation -> Relation -> Relation
relation_union rel_a rel_b = Set.union rel_a rel_b

relation_difference :: Relation -> Relation -> Relation
relation_difference rel_a rel_b = Set.difference rel_a rel_b

relation_inversion :: Relation -> Relation
relation_inversion rel = Set.map (\x -> (second x, first x)) rel

pair_join :: Pair -> Pair -> Pair
pair_join (x, y) (z, w) = (x, w)

check_composition :: Pair -> Pair -> Bool
check_composition (x, y) (z, w) | y == z = True
                                | y /= z = False

relation_composition :: Relation -> Relation -> Relation
relation_composition rel_a rel_b = Set.map (\y -> pair_join (first y) (second y)) (Set.filter (\x -> check_composition (first x) (second x)) relation_product)
    where relation_product = Set.cartesianProduct rel_a rel_b

relation_closure :: Relation -> Relation
relation_closure rel
    | relation_difference rel delta == Set.empty = rel
    | otherwise = relation_closure (relation_union rel delta)
    where delta = relation_composition rel rel

relation_append :: Pair -> Relation -> Relation
relation_append pair rel = Set.insert pair rel

count :: Relation -> Int
count relation = Set.size relation

keywords = words ") ( & | * - : = . # ? ^ > ~ < / ] ["
operators = words "& | * - : = # ^ >" -- ? .  ~ < /

brittle_map :: [String] -> String -> String
brittle_map keys [] = []
brittle_map keys (char:rest) | elem [char] keys = (" " ++ [char] ++ " ") ++ (brittle_map keys rest)
                             | otherwise = (char:(brittle_map keys rest))

brittle_tokenize :: String -> [String]
brittle_tokenize statement = words (brittle_map keywords statement)

--Thanks to fal from stackoverflow for this code
parser_helper :: [String] -> [Expression] -> (Expression, [String])
parser_helper [] as = ((Sequence as), [])
parser_helper (x : xs) as
  | x == "(" = (let (a, xs') = (parser_helper xs []) in (parser_helper xs' (as ++ [a])))
  | x == ")" = ((Sequence as), xs) 
  | elem x operators = parser_helper xs (as ++ [(Operator x)])
  | otherwise = parser_helper xs (as ++ [(Atom x)])

parse :: String -> Expression
parse statement = first (parser_helper (brittle_tokenize statement) [])

global_state :: State
global_state = Map.empty

safe_lookup :: Maybe Relation -> Relation
safe_lookup (Just r) = r
safe_lookup Nothing = Set.empty

evaluate_atom :: Expression -> State -> (Relation, State)
evaluate_atom (Atom a) s | Map.member (Atom a) s = (safe_lookup (Map.lookup (Atom a) s), s)
                         | not (Map.member (Atom a) global_state) = (Set.empty, (Map.insert (Atom a) (Set.empty) s))

-- Missing cases: ints and floats
atom_to_constant :: Expression -> Constant
atom_to_constant (Atom a) = (Individual a)

tuplify :: Expression -> Pair
tuplify (Sequence [a, b]) = ((atom_to_constant a), (atom_to_constant b))

--Input an integer and return a relation whose only member is that integer.
count_relation :: Int -> Relation
count_relation n = Set.fromList [((Integer n), (Individual "void"))]

--Some function to validate expressions is necessary (= vs : vs el resto)

--In any case, when you should output a single item,
--you can output a relation with a single constant.
evaluate :: Expression -> State -> (Relation, State)
evaluate (Atom a) s = evaluate_atom (Atom a) s
evaluate (Operator o) s = error "Bug: I was requested to evaluate an operator, but this should have not happened. "
evaluate (Sequence xs) s = case xs of
    [] -> error "Bug: I was requested to evaluate an empty list. "
    [(Operator _)] -> error "Bug: I was requested to evaluate an expression containing only an operator "
    [e1, (Operator "|"), e2] -> (relation_union (first (evaluate e1 s)) (first (evaluate e2 s)), s)
    [e1, (Operator "&"), e2] -> (relation_intersection (first (evaluate e1 s)) (first (evaluate e2 s)), s)
    [e1, (Operator "-"), e2] -> (relation_difference (first (evaluate e1 s)) (first (evaluate e2 s)), s)
    [e1, (Operator ">"), e2] -> (relation_composition (first (evaluate e1 s)) (first (evaluate e2 s)), s)
    [e1, (Operator "^")] -> (relation_inversion (first (evaluate e1 s)), s)
    [e1, (Operator "*")] -> (relation_closure (first (evaluate e1 s)), s)
    [e1, (Operator ":"), (Atom e)] -> ((relation_append (tuplify e1) (first (evaluate (Atom e) s))), (Map.insert (Atom e) (relation_append (tuplify e1) (first (evaluate (Atom e) s))) s))
    [(Operator "#"), (e1)] ->  (count_relation (count (first (evaluate e1 s))), s)
    [(Atom a), (Operator "="), e1] -> (first (evaluate e1 s), (Map.insert (Atom a) (first (evaluate e1 s)) s))
    [(Atom a)] -> (first (evaluate (Atom a) s), s)
    [(Sequence ys)] -> (evaluate (Sequence ys) s)
    _ -> error "Bug: the parser is not prepared to handle this case. "

test_loads = [ "((machirulos_en_el_espacio olga_marchetti):director)"
             , "((la_venganza_de_los_machirulos olga_marchetti):director)"
             , "((olga_en_la_mira miroslaw_szarkowski):director)"
             , "((objetivando_el_objeto miroslaw_szarkowski):director)"
             , "((esos_locos_locos_politicos omar_del_mal):director)"
             , "((a_mi_me_resbala omar_del_mal):director)"
             , "((sale_cinco_pe omar_del_mal):director)"
             , "((cuesta_la_vida omar_del_mal):director)"
             , "((loto_rojo chou_fu_fan):director)"
             , "((seda_roja chou_fu_fan):director)"
             , "((dinero_sangriento miroslaw_szarkowski):director)"
             , "((el_viejo fernanda_mercer):director)"
             , "((pololo_mololo el_gato_moncholo):director)"
             , "((sillas_sin_patas andrew_walker):director)"
             , "((machirulos_en_el_espacio accion):genero)"
             , "((machirulos_en_el_espacio feminismo):tema)"
             , "((machirulos_en_el_espacio espacio):tema)"
             , "((la_venganza_de_los_machirulos accion):genero)"
             , "((la_venganza_de_los_machirulos feminismo):tema)"
             , "((olga_en_la_mira documental):genero)"
             , "((objetivando_el_objeto documental):genero)"
             , "((esos_locos_locos_politicos documental):genero)"
             , "((a_mi_me_resbala dramas_de_la_vida):genero)"
             , "((sale_cinco_pe comedia):genero)"
             , "((cuesta_la_vida dramas_de_la_vida):genero)"
             , "((loto_rojo malardas):genero)"
             , "((seda_roja malardas):genero)"
             , "((dinero_sangriento documental):genero)"
             , "((el_viejo artistica):genero)"
             , "((pololo_mololo comedia):genero)"
             , "((sillas_sin_patas artistica):genero)"
             , "((machirulos_en_el_espacio protagonista_mujer):tema)"
             , "((machirulos_en_el_espacio espadas):tema)"
             , "((machirulos_en_el_espacio temas_politicos):tema)"
             , "((la_venganza_de_los_machirulos temas_politicos):tema)"
             , "((la_venganza_de_los_machirulos feminismo):tema)"
             , "((olga_en_la_mira farandula):tema)"
             , "((objetivando_el_objeto paja_mental):tema)"
             , "((esos_locos_locos_politicos temas_politicos):tema)"
             , "((esos_locos_locos_politicos liniers):tema)"
             , "((esos_locos_locos_politicos paja_mental):tema)"
             , "((esos_locos_locos_politicos argentina):tema)"
             , "((esos_locos_locos_politicos nisman):tema)"
             , "((a_mi_me_resbala introspeccion):tema)"
             , "((a_mi_me_resbala protagonista_machirulo):tema)"
             , "((a_mi_me_resbala dialogos_largos):tema)"
             , "((a_mi_me_resbala paja_mental):tema)"
             , "((sale_cinco_pe liniers):tema)"
             , "((sale_cinco_pe argentina):tema)"
             , "((cuesta_la_vida protagonista_machirulo):tema)"
             , "((loto_rojo chinos_raros):tema)"
             , "((seda_roja chinos_raros):tema)"
             , "((dinero_sangriento temas_politicos):tema)"
             , "((dinero_sangriento karl_marx):tema)"
             , "((el_viejo viejitos):tema)"
             , "((el_viejo ternura):tema)"
             , "((el_viejo fotografia):tema)"
             , "((pololo_mololo drogas):tema)"
             , "((pololo_mololo mas_drogas):tema)"
             , "((pololo_mololo gatos):tema)"
             , "((pololo_mololo argentina):tema)"
             , "((sillas_sin_patas sillas):tema)"
             , "(director)"
             , "((director^)>tema)"
             , "((paja_mental empty):paj)"
             , "(((director^)>tema)>paj)"
             , "((temas_politicos empty):pol)"
             , "(((director^)>tema)>pol)" ]

succesive_evaluation :: [(Expression, String)] -> State -> [((Relation, State), String)]
succesive_evaluation [] s = []
succesive_evaluation (x:xs) s = (((evaluate (first x) s), (second x)):(succesive_evaluation xs (second (evaluate (first x) s))))

evaluations = succesive_evaluation (map (\x -> (parse x, x)) test_loads) global_state

show_relation :: Relation -> String
show_relation rel | Set.null rel = "This relation is empty." ++ "\n \n"
show_relation rel | otherwise = "\n" ++ (concat (map (\x -> (show_pair x)) (Set.elems rel))) ++ "\n \n"

show_evaluation :: ((Relation, State), String) -> String
show_evaluation ((r, s), e) = e ++ "\n" ++ (show_relation r)

test_statement_1 = "(#(((a^)|b)*))"
test_statement_2 = "(show (a|b))"
test_statement_3 = "(exit)"
test_statement_4 = "(r = (((a^)|(b^))*))"
test_statement_5 = "(#(((a^)|b)*))"

show_pair :: Pair -> String
show_pair (a, b) = "(" ++ (show_constant a) ++ " " ++ (show_constant b) ++ ")" ++ "\n"

show_constant :: Constant -> String
show_constant (Individual s) = show s
show_constant (Floating f) = show f
show_constant (Integer i) = show i
show_constant (Boolean b) = show b 

read_ :: IO String
read_ = putStr "RDB REPL> "
    >> hFlush stdout
    >> getLine

repl_pipeline :: State -> String -> IO String
repl_pipeline state expr = do  current_input <- read_
                               let current_expression = (parse current_input)
                               let current_evaluation = evaluate current_expression state
                               let current_state = second (current_evaluation)            
                               putStr (show_relation (first current_evaluation)) >> repl_ current_state current_input

repl_ :: State -> String -> IO String
repl_ state expr | expr == "(tutorial)" = do forM_ (map show_evaluation evaluations) putStr
                                             let current_state = (second (first (last evaluations)))
                                             repl_pipeline current_state "ok"
                 | expr == "(exit)" = do return "Exiting RDB..."
                 | expr == "(clear)" = do repl_pipeline Map.empty "ok"
                 | otherwise = repl_pipeline state expr

main = do repl_ Map.empty "ok"
