val listavuota = [];
val lista = [1,2];

null listavuota; (*True se vuota, false se vuota*)

hd [1,2]; (* Head *)
tl [1,2]; (* Tail *)
length [2,3]; (* Lunghezza *)

val x = [5,1,8,6];
val otto = hd(tl(tl(x)));


fun prova (a) = a+2;
(* >val prova = fn: int -> int *)
fun prova (a) = 2;
(* >val prova = fn: 'a -> int | dove 'a significa qualsiasi tipo. *)
fun sommaCoppie (a:int*int, b:int*int) = #1 a + #1 b;

type coppia = int*itn;
fun sommaCoppie (a:coppia, b:coppia) = #1 a + #1 b;

(* Prende una coppia (bool, int) e ritorna una coppia (int, bool) *)
fun scambia (a:bool*int) = (#2 a, #1 a);

fun test a = lenght a * 2; (*>val test = fn: 'a list -> int *)
