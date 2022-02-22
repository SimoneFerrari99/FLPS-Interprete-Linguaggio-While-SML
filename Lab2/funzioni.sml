(* def nome_funzione (parametri): return (...) *)


fun quadrato(x:int) = 
    x*x;
    
fun potenza(a:int, b:int) = 
    if b < 0 
    then a
    else a * potenza(a, b-1);

(* >val potenza = fn: int * int -> int *)

fun cubo(x:int) =
    x*x*x

fun cuboWithPow(x:int) =
    potenza(x, 3)

val res = potenza(x, 3);

fun fibonacci(n:int) = 
    if n<=2 then 1
    else fib(n-1) + fib(n-2)

fun fib(n:int) = 
    case (n) of
    0 => 0
    | 1 => 1
    | n => fib(n-1) + fib(n-2)