Int
String
List
...

datatype nuovotipo = 
    coppia of int * int
    | vuoto;

val a = vuoto;
val b = coppia(1+1, 1+2);
val c = coppia();

coppia(if2>1 then 2 else 3, 4);

fun extract(a:nuovotipo) = 
    case a of
        coppia(i1, i2) => 1
        | vuoto => 2