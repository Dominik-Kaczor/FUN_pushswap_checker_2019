--
-- EPITECH PROJECT, 2020
-- pushswap_checker
-- File description:
-- checker of pushswap
--

import System.Environment
import System.Exit

checkInt :: [Char] -> Bool
checkInt x
    | all(>='0') x == True && all (<='9') x == True = True
    | otherwise = False

checkNeg :: Char -> Bool
checkNeg x
    | x == '-' = True
    | otherwise = False

readInt  :: [Char] -> Maybe  Int
readInt [] = Nothing
readInt (a:b) = 
    if checkNeg a == True
        then if checkInt b == True
                then Just (read(a:b))
             else Nothing
    else if checkInt (a:b)
        then Just (read(a:b))
    else Nothing

check_Int  ::  [[Char]] -> Int -> Maybe Int
check_Int (args:as) nb  =   if nb == 0
                                then Just 1
                            else
                                if readInt args == Nothing
                                    then Nothing
                                else
                                    check_Int as (nb - 1)

tab_to_Int  ::  [[Char]] -> Int -> [Int]
tab_to_Int (args:as) nb =   if nb == 1
                                then (read args):[]
                            else
                                (read args):tab_to_Int as (nb - 1)


check_params  ::  [Char] -> Bool
check_params args = if "sa" == args || "sb" == args || "sc" == args || "pa" == args || "pb" == args || "ra" == args || "rb" == args || "rr" == args || "rra" == args || "rrb" == args || "rrr" == args
                        then True
                    else False

check_arguments  ::  [[Char]] -> Int -> IO ()
check_arguments (args:as) nb =  if nb == 1
                                    then if check_params args == True
                                            then return ()
                                        else exitWith $ ExitFailure 84
                                else if nb == 2
                                    then if check_params args == True && check_params (last (args:as))
                                            then return ()
                                        else exitWith $ ExitFailure 84
                                else
                                    if check_params args == True
                                        then check_arguments as (nb - 1)
                                    else exitWith $ ExitFailure 84

check_sort  ::  [Int] -> ([Int], [Int]) -> Int -> IO()
check_sort (la:ls) tup nb = if la > (head ls)
                                then do 
                                    putStr "KO: "
                                    print tup
                            else
                                if nb == 2
                                    then putStrLn "OK"
                                else
                                    check_sort ls tup (nb - 1)

inv_s  ::  [Int] -> [Int]
inv_s [] = []
inv_s (a:b) = (head b):a:(tail b)

la_to_lb  ::  [Int] -> [Int] -> ([Int], [Int])
la_to_lb [] c = ([], c)
la_to_lb (a:b) c = (b, a:c)

lb_to_la  ::  [Int] -> [Int] -> ([Int], [Int])
lb_to_la a [] = (a, [])
lb_to_la a (c:d) = (c:a, d)

fst_to_lst  ::  [Int] -> [Int]
fst_to_lst [] = []
fst_to_lst (a:b) = b ++ [a]

lst_to_fst  :: [Int] -> [Int]
lst_to_fst [] = []
lst_to_fst (a:b) = last(a:b):init(a:b)

modify_list  ::  [Int] -> [Int] -> [Char] -> ([Int], [Int])
modify_list la lb cmd = if cmd == "sa"
                            then (inv_s la, lb)
                        else if cmd == "sb"
                            then (la, inv_s lb)
                        else if cmd == "sc"
                            then (inv_s la, inv_s lb)
                        else if cmd == "pa"
                            then lb_to_la la lb
                        else if cmd == "pb"
                            then la_to_lb la lb
                        else if cmd == "ra"
                            then (fst_to_lst la, lb)
                        else if cmd == "rb"
                            then (la, fst_to_lst lb)
                        else if cmd == "rr"
                            then (fst_to_lst la, fst_to_lst lb)
                        else if cmd == "rra"
                            then (lst_to_fst la, lb)
                        else if cmd == "rrb"
                            then (la, lst_to_fst lb)
                        else 
                            (lst_to_fst la, lst_to_fst lb)

exec_cmd  ::  ([Int], [Int]) -> [[Char]] -> Int -> IO()
exec_cmd a (c:d) nb =   if nb == 1
                            then do
                                let ter = (modify_list (fst a) (snd a) c)
                                if snd ter /= []
                                    then do
                                        putStr "KO: "
                                        print ter
                                else
                                    check_sort (fst ter) ter (length (fst ter))
                        else exec_cmd (modify_list (fst a) (snd a) c) d (nb - 1)

main = do
    args <- getArgs
    if args == []
        then exitWith $ ExitFailure 84
    else if check_Int args ((length args) - 1) == Nothing || readInt (last args) == Nothing
        then exitWith $ ExitFailure 84
    else do
        let tbl = tab_to_Int args (length args)
        cmd <- getLine
        if cmd == []
            then check_sort tbl (tbl, []) (length tbl)
        else do
            check_arguments (words cmd) (length (words cmd))
            if (length tbl == 1)
                then putStrLn "OK"
            else exec_cmd (tbl, []) (words cmd) (length (words cmd))