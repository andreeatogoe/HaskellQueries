module Query where

import UserInfo
import Rating
import Movie
import Data.List

type Column = String
type TableSchema = [Column]
type Field = String
type Entry = [Field]

data Table = Table TableSchema [Entry]

type ColSeparator = Char
type LnSeparator = Char

-- TODO 1
read_table :: ColSeparator -> LnSeparator -> String -> Table
read_table col line [] = Table [] []
read_table col line str = Table (head (split col line str)) (tail (split col line str))

--separa string-ul citit in linii si coloane
split :: ColSeparator -> LnSeparator -> String -> [[String]]
split col line s = (split_col col (split_lines line s))
    
--separa String-ul in linii
split_lines :: LnSeparator -> String -> [String]
split_lines l [] = [[]]
split_lines l s
              | (head s) == l = "" : (split_lines l (tail s))
              | otherwise = ((head s) : head (split_lines l (tail s))) : tail (split_lines l (tail s)) 

--separa String-urile din lista in coloane
split_col :: ColSeparator -> [String] -> [[String]]
split_col c [[]] = []
split_col c s = (split_lines c (head s)) : (split_col c (tail s))


user_info = (read_table '|' '\n' user_info_str)
rating = (read_table ' ' '\n' rating_str)
movie = (read_table '|' '\n' movie_str)

-- TODO 2
instance Show Table where
    show (Table header entries)
                              | header == [] = "\n"
                              | otherwise = (add_line (max_len (Table header entries))) ++ "\n" ++ (show_line header (max_len (Table header entries))) ++ (add_line (max_len (Table header entries))) ++ "\n" ++ (show_entries entries (max_len (Table header entries))) ++ "\n"

--functie pentru afisarea liniilor 
show_line :: [String] -> [Integer] -> String
show_line [] l = "|\n"
show_line (x:xs) l = "|" ++ x ++ (add_spaces (calc_dif x (head l))) ++ (show_line xs (tail l))

--functie pentru afisarea fiecarei linii din entries
show_entries :: [Entry] -> [Integer] -> String
show_entries [] l = add_line l
show_entries (e:ex) l = (show_line e l) ++ (show_entries ex l)

--construieste un string de "-"
add_line :: [Integer] -> String
add_line [] = line 1
add_line (l:ls) = (add_line ls) ++ line l ++ line 1

line :: Integer -> String
line 0 = ""
line x = "-" ++ (line (x - 1))

--functie de construieste un string de spatii pentru padding
add_spaces :: Integer -> String
add_spaces 0 = ""
add_spaces x = " " ++ (add_spaces (x - 1))

--returneaza o lista de int ce contine diferentele dintre lungimea maxima a 
--unei coloane si lungimea unui Field din coloana respectiva
calc_dif :: String -> Integer -> Integer
calc_dif s i = i - (count s)

--returneaza o lista cu lungimea maxima de pe fiecare coloana a lui table
max_len :: Table -> [Integer]
max_len (Table header entries) = choice (line_count header) (max_entry entries)

--returneaza o lista cu lungimea max de pe fiecare coloana a listei de entries
max_entry :: [Entry] -> [Integer]
max_entry [] = []
max_entry (x:xs) = choice (line_count x) (max_entry xs)

--returneaza maximul dintre cele doua liste de la fiecare pozitie
choice :: [Integer] -> [Integer] -> [Integer]
choice [] _ = []
choice l [] = l
choice l1 l2
           | (head l1) > (head l2) = (head l1) : (choice (tail l1) (tail l2))
           | otherwise = (head l2) : (choice (tail l1) (tail l2))

--returneaza o lista cu lungimile fiecarui string din lista data
line_count :: [String] -> [Integer]
line_count [] = []
line_count (s:sx) = (count s) : (line_count sx) 

--returneaza lungimea string-ului dat ca parametru
count :: String -> Integer
count [] = 0
count s = 1 + (count (tail s))


data FilterCondition = Lt Field Integer | Eq Field String | In Field [String] | Not FilterCondition

-- TODO 3
getFilter :: FilterCondition -> TableSchema -> (Entry -> Bool)
getFilter (Not filter) header  = f
                      where f [] = True
                            f entry = not (verif header entry filter)
getFilter filter header = f
                      where f [] = False
                            f entry = (verif header entry filter)

--functie care cauta pozitia aferenta field-ului dat si verifica conditia
verif :: TableSchema -> Entry -> FilterCondition -> Bool
verif [] _ _ = False
verif h e (Lt f i)
                 | (((head h) == f) && ((read (head e) :: Integer) < i)) = True
                 | (((head h) == f) && ((read (head e) :: Integer) >= i)) = False
                 | otherwise = verif (tail h) (tail e) (Lt f i)
verif h e (Eq f s)
                 | ((head h == f) && (head e == s)) = True
                 | ((head h == f) && (head e /= s)) = False
                 | otherwise = verif (tail h) (tail e) (Eq f s)
verif h e (In f s)
                 | (head h) == f = cond_in (head e) s
                 | otherwise = verif (tail h) (tail e) (In f s)
verif h e (Not filter) = not (verif h e filter) 

--verificarea conditiei pt in
cond_in :: Field -> [String] -> Bool
cond_in f [] = False
cond_in f (s:sx) 
               | f == s = True
               | otherwise = cond_in f sx

-- TODO 4
data Query = Filter FilterCondition Query |  
             Select [String] Query |
             SelectLimit [String] Integer Query |
             Cosine Query |
             Query :|| Query |
             Atom Table

eval :: Query -> Table
eval (Atom x) = x
eval (Select headers x) = Table headers (select headers (eval x))
eval (SelectLimit headers i x) = Table headers (select_lim headers i (eval x))
eval (Filter f x) = Table (get_header (eval x)) (filter (getFilter f (get_header (eval x))) (get_entries (eval x)))
eval (x :|| y) = lipire (eval x) (eval y)

--returneaza headerul tabelului primit
get_header :: Table -> TableSchema
get_header (Table h e) = h

--returneaza entry-urile tabelului primit
get_entries :: Table -> [Entry]
get_entries (Table h e) = e

x = (Select ["title","movie_id"] (Atom movie))

--functie ce cauta fiecare string primit in lista, in header, si
--returneaza o lista de entry-uri ce corepund stringurilor
select :: [String] -> Table -> [Entry]
select [] _ = []
select (h:hs) (Table header entries) = merge (find_col h (Table header entries)) (select hs (Table header entries))

--functie ce cauta fiecare string primit in lista, in header, si
--returneaza o lista de entry-uri ce corepund stringurilor, cu doar i linii
select_lim :: [String] -> Integer -> Table -> [Entry]
select_lim [] _ _ = []
select_lim (h:hs) i (Table header entries) = merge (find_lim h i (Table header entries)) (select_lim hs i (Table header entries))
 
--functia gaseste coloana corespunzatoare stringului si o returneaza
find_col :: String -> Table -> [String]
find_col s (Table [] _) = []
find_col s (Table h e)
                 | s == (head h) = map head e
                 | otherwise = find_col s (Table (tail h) (map tail e))

--functia gaseste coloana corespunzatoare stringului si returneaza primele i elemente
find_lim :: String -> Integer -> Table -> [String]
find_lim s i (Table [] _) = []
find_lim s i (Table h e)
                       | s == (head h) = (take_i i (map head e))
                       | otherwise = find_lim s i (Table (tail h) (map tail e))

--functie ce selecteaza primele i elemente dintr o lista
take_i :: Integer -> [String] -> [String]
take_i i [] = []
take_i 0 _ = []
take_i i (s:xs) = s : (take_i (i - 1) xs)

--functia primeste o coloana si o lista de entry, si o lipeste la inceputul listei
merge :: [String] -> [[String]] -> [[String]]
merge [] e = e
merge l [] = [[head l]] ++ (merge (tail l) []) 
merge (l:ls) (e:ex) = (l : e) : (merge ls ex)

lipire :: Table -> Table -> Table
lipire (Table h1 e1) (Table h2 e2) = (Table h1 (e1 ++ e2))

-- TODO 5
same_zone :: String -> Query
same_zone user = Atom (eval (Select ["user_id", "occupation"] (Filter (Eq "zone" (get_zone user user_info)) (Filter (Not (Eq "user_id" user)) (Atom user_info)))))

--functie ce returneaza valoare campului zone pentru user_id-ul dat
get_zone :: String -> Table -> String
get_zone user (Table h []) = ""
get_zone user (Table h e)
                        | (head (head e)) == user = (last (head e))
                        | otherwise = get_zone user (Table h (tail e))

male_within_age :: Integer -> Integer -> Query
male_within_age x y = Atom (eval (Select ["occupation", "zone"] (Filter (Eq "sex" "M") ((Filter (Lt "age" y) (Filter (Not (Lt "age" (x + 1))) (Atom user_info))) ) )  ))

mixed :: [String] -> [String] -> Int -> Query
mixed zones jobs x = Atom (eval (Select ["user_id"] (Filter (In "zone" zones) (Filter (In "occupation" jobs) (Filter (Lt "age" (toInteger x)) (Atom user_info))))))
