(* 
AS: Ambiente Statico;
AD: Ambiente Dinamico;
*)

val x = 1; (* Binding variabile *)
(* AS: x:int *)
(* AD: x=1 *)

val y = 2;
(* AS: x:int, y:int *)
(* AD: x=1, y=2 *)

val z = x + 1;
(* AS: x:int, y:int, z:(int*int):int *)
(* AD: x=1, y=2, z=2 *)

val t = (x + y);
(* AS: x:int, y:int, z:(int*int):int, t:(int*int):int *)
(* AD: x=1, y=2, z=2, t=3 *)

val a = 70;
val assoluto = if a < 0 then 0-a else a;
(* Sintassi: come lo scrivo? *)
(* Semantica: TypeChecking e EvaluationRule *)
(* if e1 then e2 else e3 *)
(* if, else sono keywords *)
(* e1:bool, e2:t, e3:t *)

(* AS: ..., a:int,  *)
(* AD: ..., a=70,   *)