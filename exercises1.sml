(* fun test (func, convertFunc, inputList) =
    let 
        fun helper (phrase, []) = print (phrase ^ "\n") 
        |   helper (phrase, (a::L)) = helper (phrase ^ " " ^ (convertFunc (func a)), L)
    in
        helper("", inputList)
    end;

fun testWithInput (func, outputConvertFunc, inputConvertFunc, inputList) =
    let 
        fun helper (phrase, []) = print (phrase ^ "\n") 
        |   helper (phrase, (a::L)) = helper (phrase ^ " (" ^ (inputConvertFunc a) ^ ") " ^ (outputConvertFunc (func a)), L)
    in
        helper("", inputList)
    end;

val maxInt = valOf Int.maxInt;
val minInt = valOf Int.minInt;

val intTestSuit = [~10,~9,~8,~7,~6,~5,~4,~3,~2,~1,0,1,2,3,4,5,6,7,8,9,10];
val intTestSuitPOS = [0,1,2,3,4,5,6,7,8,9,10];
val intTestSuitMINMAX = [minInt, ~10,~9,~8,~7,~6,~5,~4,~3,~2,~1,0,1,2,3,4,5,6,7,8,9,10,maxInt];

val intListTestSuit = [[~1,~2,~3,~4], [~4, ~2, 0, 2, 4], [1,2,3,4,5], [~3,4,~6,~1,6,0,8]] *)

fun min (x:int,y:int) = if x < y then x else y

(* 
test (min, Int.toString, [(1,2), (2,1), (2,2), (~1,1), (minInt, maxInt)]); *)

(* min (1,2);
min (2,1);
min (2,2);
min (~1,1);  *)

fun fib 0 = 0
|   fib (x:int) =
    let
        fun helper (prev1, prev2, 0) = prev1
        |   helper (prev1, prev2, iteration) = helper(prev2, prev1 + prev2, iteration - 1)
    in
        helper(0,1,x)
    end
    

(* test (fib, Int.toString, [0,1,2,3,4,5,6,7,8,9]); *)
(* testWithInput (fib, Int.toString, Int.toString, intTestSuitPOS) *)

fun isPrime 1 = false
|   isPrime 2 = true
|   isPrime (x:int) =
    let 
        val testCases = [2,3,4,5,6,7,8,9]
        fun helper [] = true 
        |   helper (a::L) = if x <> a andalso (x mod a) = 0 then false else (helper L)
    in
        x > 0 andalso helper testCases
    end

(* testWithInput (isPrime, Bool.toString, Int.toString, intTestSuit) *)

fun sumList (x:int list) = foldl (op +) 0 x

(* sumList [0,1,2,3,4,5];
sumList [~1,~2,~3];
sumList [~1,~2,~3, 0, 3, 2, 1]; *)
(* sumList []; *)

fun squareList (x:int list) = map (fn x => x * x) x
(* squareList [0,1,2,3,4,5];
squareList [~1,~2,~3];
squareList [~1,~2,~3, 0, 3, 2, 1]; *)
(* squareList []; *)
