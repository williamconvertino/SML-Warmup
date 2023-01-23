datatype expr =
NUM of int
| PLUS of expr * expr
| MINUS of expr * expr
| TIMES of expr * expr
| DIV of expr * expr
| F of expr list * (int list -> int)

fun eval (NUM n) = n
|   eval (PLUS (n1, n2)) = eval(n1) + eval(n2)
|   eval (MINUS (n1,n2)) = eval(n1) - eval(n2)
|   eval (TIMES (n1,n2)) = eval(n1) * eval(n2)
|   eval (DIV (n1,n2)) = eval(n1) div eval(n2)
|   eval (F (exprs, func)) = func (map eval exprs)

(* val test = eval (NUM 4);
val test = eval (DIV ((TIMES ((NUM 8), (MINUS ((PLUS ((NUM 4),(NUM 5))), (NUM 6))))), (NUM 8)));

(* 3 + 4 + 5 *)
val exps = [(DIV ((TIMES ((NUM 8), (MINUS ((PLUS ((NUM 4),(NUM 5))), (NUM 6))))), (NUM 8))), (NUM 4), (NUM 5)];
val test = eval (F (exps, fn e => foldl (op +) 0 e));
val test = eval (F ([], fn e => foldl (op +) 0 e)); *)

fun flatten (lst:int list list) = 
    let
        fun helper (a,L) = foldr (op ::) L a
    in
        foldr helper [] lst
    end

(* val test = flatten  [[1,2,3], [4], [5,6], [], [7]]
val test = flatten  [[1], [2], [3], [4], [5]]
val test = flatten  [[], [], [], [], []]
val test = flatten  [] *)

fun foldmap (func: 'a -> 'b): 'a list -> 'b list =  
    let 
        fun helper (a, L) = (func a) :: L
    in
        fn (x: 'a list) => foldr (helper: 'a * 'b list -> 'b list) [] x
    end

(* val test = foldmap (fn e => e * e) [1,2,3,4]
val test = foldmap (fn e => e + 1) [1,2,3,4]
val test = foldmap (fn e => e) [1,2,3,4]
val test = foldmap (fn e => e) [] *)

fun foldfilter (func: 'a -> bool): 'a list -> 'a list =  
    let 
        fun helper (a, L) = if (func a) then a :: L else L
    in
        fn  (x: 'a list) => foldr helper [] x
    end

(* val test = foldfilter (fn e => e = 3) [1,2,3,4]
val test = foldfilter (fn e => e > 1) [1,2,3,4]
val test = foldfilter (fn e => true) [1,2,3,4]
val test = foldfilter (fn e => false) [1,2,3,4]
val test = foldfilter (fn e => true) [] *)

fun count (func: 'a -> bool): 'a list -> int =
    let
        fun helper (a: 'a, count: int) = if (func a) then count + 1 else count 
    in
        fn (lst:'a list) => foldl helper 0 lst
    end

(* val test = count (fn e => e > 2) [~1,~2,~3,0,1,2,3,4]
val test = count (fn e => e = 2) [~1,~2,~3,0,1,2,3,4]
val test = count (fn e => e < 2) [~1,~2,~3,0,1,2,3,4]
val test = count (fn e => e < 2) []
val test = count (fn e => e = []) [[],[],["a", "b"], [], ["c"]]
val test = count (fn e => e = []) [] *)

fun mapPartial (func: 'a -> 'b option):'a list -> 'b list =
    let
        fun helper (a: 'a, L: 'b list) = 
            case (func a) of
                (SOME b) => b::L
            |   (NONE) => L
    in
        fn (lst: 'a list) => foldr helper [] lst
    end
(* 
mapPartial (fn e => if e > 2 then SOME ("Success: " ^ (Int.toString e)) else NONE) [1,2,3,4,5]
mapPartial (fn e => if e = 6 then SOME ("Success: " ^ (Int.toString e)) else NONE) [6,6,1,2,3,4,5,6]
mapPartial (fn e => NONE) [6,6,1,2,3,4,5,6]
mapPartial (fn e => if e = 6 then SOME ("Success: " ^ (Int.toString e)) else NONE) [] *)

