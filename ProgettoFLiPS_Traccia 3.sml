(* SINTASSI *)
datatype tipiBase =  int | bool | unit | proc
datatype ope = somma | maggug
type loc = string
datatype tipoloc = intref

datatype exp =
	Integer of int
  | Boolean of bool
  | Op of  exp * ope * exp
  | If of exp * exp * exp
  | Assign of loc * exp
  | Deref of loc
  | Skip
  | Seq of exp * exp
  | While of exp * exp
  | Par of exp * exp
  | Choice of exp * exp
  | Await of exp * exp

(* FUNZIONI DI VARIA UTILITA' *)
fun isValue(Integer x) = true
  | isValue(Boolean x) = true
  | isValue(Skip) = true
  | isValue(_) = false

fun controllo([], l) = NONE
  | controllo((l', n')::pairs, l) = 
		if l=l' then SOME n' 
				else controllo(pairs, l)

fun upsup testa [] (l,n) = NONE
  | upsup testa ((l',n')::pairs) (l,n) =
		if l=l' then SOME(testa @ ((l,n)::pairs))
				else upsup ((l',n')::testa) pairs (l, n)

fun up((l,n), s) = upsup [] s (l,n)

val randomRange = Random.randRange(0,2)
val random = Random.rand()

(* SEMANTICA - Small Step *)
  (* INT *)
fun riduzione (Integer n, s) = NONE
  (* BOOL *)
  | riduzione (Boolean n, s) = NONE
  (* OP + E >= *)
  | riduzione (Op(e1, ope, e2), s) = (
		case (e1,ope,e2) of
			(Integer i1, somma, Integer i2) => SOME(Integer(i1+i2), s)              (* OP + *)
			|(Integer i1, maggug, Integer i2) => SOME(Boolean(i1>=i2), s)           (* OP >= *)
			|(e1, ope, e2) => (
				if(isValue e1) then (
					case riduzione(e2,s) of
						SOME(e2', s') => SOME(Op(e1,ope,e2'), s')                   (* OP 2 *)
						|NONE => NONE
				) else (
					case riduzione(e1,s) of
						SOME(e1', s') => SOME(Op(e1',ope,e2), s')                   (* OP 1 *)
						|NONE => NONE
				)
			)
	)
  (* IF THEN ELSE *)
  | riduzione (If(e1, e2, e3), s) = (
		case e1 of
			Boolean(true) => SOME(e2, s)                                            (* IF-TT *)
			|Boolean(false) => SOME(e3, s)                                          (* IF-FF *)
			|_ => (
				case riduzione(e1,s) of
					SOME(e1', s') => SOME(If(e1', e2, e3), s')                      (* IF *)
					|NONE => NONE
			)
	)
  (* SKIP *)
  | riduzione (Skip, s) = NONE
  (* COMPOSIZIONE SEQUENZIALE *)
  | riduzione (Seq(e1,e2), s) = (               
		case e1 of 
			Skip => SOME(e2, s)                                                     (* SEQ-SKIP *)
			|_ => ( 
				case riduzione(e1,s) of
					SOME(e1', s') => SOME(Seq(e1',e2), s')                          (* SEQ *)
					|NONE => NONE
			)
	)
  (* WHILE *)
  | riduzione (While(e1,e2), s) = SOME(If(e1, Seq(e2, While(e1, e2)), Skip),s)      (* WHILE *)
  (* DEREF *)
  | riduzione (Deref(l), s) = (
		case controllo(s,l) of
			SOME x => SOME(Integer x, s)                                            (* DEREF *)
			|NONE => NONE
	)
  (* ASSIGN *)
  | riduzione (Assign(l,e), s) = (
		case e of
			Integer n => ( 
				case up((l,n), s) of
					SOME(s') => SOME(Skip, s')                                      (* ASSIGN 1 *)
					|NONE => NONE
			)
			|_ => ( 
				case riduzione(e,s) of
					SOME(e', s') => SOME(Assign(l, e'), s')                         (* ASSIGN 2 *)
					|NONE => NONE 
			)
	)
  (* COMPOSIZIONE PARALLELA *)
  | riduzione(Par(e1,e2), s) = (                                                    
		if ((randomRange random) = 0) then ( (* Scelta randomRange (0.5 probabilità) *)
			case e1 of 
				Skip => SOME(e2, s)                                                 (* END L *)
				|_ => (
					case riduzione(e1, s) of                        
						SOME(e1', s') => SOME(Par(e1', e2), s')                     (* PAR L *)
						|NONE => (
						    case e2 of
						        Skip => SOME(Par(e1,e2),s)
						        |_ => (
							        case riduzione(e2, s) of
								        SOME(e2',s') => SOME(Par(e1, e2), s)
								        |NONE => NONE
								)
						)
				)
		) else (
			case e2 of 
				Skip => SOME(e1, s)                                                 (* END R *)
				|_ => (
					case riduzione(e2, s) of                        
						SOME(e2', s') => SOME(Par(e1, e2'), s')                     (* PAR R *)
						|NONE => (
						    case e1 of
						        Skip => SOME(Par(e1,e2),s)
						        |_ => (
        							case riduzione(e1, s) of
        								SOME(e1',s') => SOME(Par(e1, e2), s)
        								|NONE => NONE
        						)
						)
				)
		)
	)
  (* SCELTA NON DETERMINISTICA *)
  | riduzione(Choice(e1,e2), s) = (                                                    
		if ((randomRange random) = 0) then ( (* Scelta randomRange (0.5 probabilità) *)
			case e1 of
			    Skip => NONE
			    |_ => (
        			case riduzione(e1, s) of                        
        				SOME(e1', s') => SOME(e1', s')                                      (* CHOICE L *)
        				|NONE => (
        				    case e2 of
        				        Skip => SOME(Choice(e1,e2), s)
        				        |_ => (
                					case riduzione(e2, s) of
                						SOME(e2',s') => SOME(Choice(e1, e2), s)
                						|NONE => NONE
                				)
        		
				        )
				)
		) else (
		    case e2 of
		        Skip => NONE
		        |_ => (
        			case riduzione(e2, s) of                        
        				SOME(e2', s') => SOME(e2', s')                                      (* CHOICE R *)
        				|NONE => (
        				    case e1 of
        				        Skip => SOME(Choice(e1,e2), s)
        				        |_ => (
                					case riduzione(e1, s) of
                						SOME(e1',s') => SOME(Choice(e1, e2), s)
                						|NONE => NONE
                				)
        				)
        		)
        )
	)
  (* AWAIT *)
  | riduzione(Await(e1,e2), s) = (
	    case e1 of
			Boolean(true) => (
				case e2 of
					Skip => SOME(Skip, s)
					|_ => (
						case riduzione(e2, s) of
							SOME(e2', s'') => riduzione(Await(e1,e2'), s'')
							|NONE => NONE
					)
			)
			|_ => (
				case riduzione(e1, s) of
					SOME(e1', s') => riduzione(Await(e1', e2), s')
					|NONE => NONE
			)
    )
	

(* SEMANTICA - Big Step *)
fun bigs(e,s) = 
		case riduzione(e,s) of
			SOME(e',s') => bigs(e',s')
			|NONE => (e,s)


(* TYPE CHECKER *)
fun typecheck gamma (Integer x) = SOME(int)
  | typecheck gamma (Boolean b) = SOME(bool)
  | typecheck gamma (Skip) = SOME(unit)
  | typecheck gamma (Op(e1, ope, e2)) = (
		case (typecheck gamma e1, ope, typecheck gamma e2) of
			(SOME int, somma, SOME int) => SOME(int)
			|(SOME int, maggug, SOME int) => SOME(bool)
			|_ => NONE
	)
  | typecheck gamma (While(e1, e2)) = (
		case (typecheck gamma e1, typecheck gamma e2) of
			(SOME bool, SOME unit) => SOME(unit)
			|_ => NONE
	)
  | typecheck gamma (Assign(l,e)) = (
		case (controllo(gamma, l), typecheck gamma e) of
			(SOME intref, SOME int) => SOME(unit)
			|_ => NONE
	)
  | typecheck gamma (Deref(l)) = (
		case (controllo(gamma, l)) of
			(SOME intref) => SOME(int)
			|_ => NONE
	)
  | typecheck gamma (If(e1,e2,e3)) = (
		case (typecheck gamma e1, typecheck gamma e2, typecheck gamma e3) of
			(SOME bool, SOME t2, SOME t3) => ( if t2=t3 then SOME(t2) else NONE )
			|_ => NONE
	)
  | typecheck gamma (Seq(e1, e2)) = (
		case (typecheck gamma e1, typecheck gamma e2) of
			(SOME unit, SOME unit) => SOME(unit)
			|(SOME unit, SOME proc) => SOME(proc)
			|_ => NONE
	)
  | typecheck gamma (Par(e1, e2)) = (
		case (typecheck gamma e1, typecheck gamma e2) of
			(SOME unit, SOME unit) => SOME(proc)
			|(SOME unit, SOME proc) => SOME(proc)
			|(SOME proc, SOME unit) => SOME(proc)
			|(SOME proc, SOME proc) => SOME(proc)
			|_ => NONE
	)
  | typecheck gamma (Choice(e1, e2)) = (
		case (typecheck gamma e1, typecheck gamma e2) of
			(SOME unit, SOME unit) => SOME(unit)
			|_ => NONE
	)
  | typecheck gamma (Await(e1, e2)) = (
		case (typecheck gamma e1, typecheck gamma e2) of
			(SOME bool, SOME unit) => SOME(unit)
			|_ => NONE
	)


(* PRETTY PRINT *)
open Int
open Listsort

fun     stampaop somma = "+"
	|   stampaop mu = ">="

fun     stampalw (Integer x) = Int.toString x
	|   stampalw (Boolean b) = if b then "true" else "false"
	|   stampalw (Op(e1, ope, e2)) = "[" ^ stampalw e1 ^ stampaop ope ^ stampalw e2 ^ "]"
	|   stampalw (While(e1, e2)) = "while " ^ stampalw e1 ^ " do " ^ stampalw e2
	|   stampalw (Skip) = "(skip) "
	|   stampalw (If(e1,e2,e3)) = "if " ^ stampalw e1 ^ " then " ^ stampalw e2 ^ " else " ^ stampalw e3
	|   stampalw (Deref(l)) = "!" ^ l
	|   stampalw (Assign(l, e)) = l ^ ":=" ^ stampalw e
	|   stampalw (Seq(e1,e2)) = stampalw e1 ^ "; " ^ stampalw e2
	|   stampalw (Par(e1,e2)) = stampalw e1 ^ " || " ^ stampalw e2
	|   stampalw (Choice(e1,e2)) = stampalw e1 ^ " (+) " ^ stampalw e2
	|   stampalw (Await(e1,e2)) = "await " ^ stampalw e1 ^ " protect " ^ stampalw e2 ^ " end"