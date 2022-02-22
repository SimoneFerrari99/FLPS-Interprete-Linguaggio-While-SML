(* Tipi base *)
unit      it;
int       1;    1 div 3;
real      1.2;  1.2/3.1;
string    "ciao";   "ciao"^" come stai?";
bool      true;
char      #"d";

(* If *)
if e1 then e2 else e3;
> e1: bool, e2: 'a, e3: 'a

(* Variabili *)
val x = 10; (* binding *)
AS x:int;
AD x=10;

x=12 (* controllo se x = 12*)
val x = 12; (* modifico x *)

(* Funzioni *)
val sconto = fn(x:int, y:int) => (x*y) div 100;
fun sconto (x:int, y:int) = (x*y) div 100;

(* Funzioni ricorsive *)
val rec sconto = fn(x:int, y:int) => (x*y) div 100 * sconto ....;

(* Let *)
val costo = 10;
fun costoso(a:int) = 
    let
        val costo = 1000;
    in 
        sconto costo 10;
    end;

(* Case *)
fun is_uno(a) = 
    case a of
    1  => true
    |_ => false;

(* Datatype *)
datatype test = 
    niente
    | testo of string
    | coppia of int*int

fun extract (z:test) =
    case z of
    niente >= "niente"
    | testo => "testo"
    | coppia(_,_) => "coppia)"