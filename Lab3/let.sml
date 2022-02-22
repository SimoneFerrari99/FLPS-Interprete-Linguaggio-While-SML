
(*let binding, b2... IN expr end*)

(* val rec prova = fn ... *)
fun prova (a:int) =
	let 
		val x = if a>1 then a else 2
		(* AD a=2, x=2 *)
		val y = x+1
		(* AD a=2, x=2, y=3 *)
	in
		if x>y then x+1 else y+1
		(* AD a=2, x=2, y=3 *)
	end

(* Statico >val prova: fn int -> int*)

prova(2);
