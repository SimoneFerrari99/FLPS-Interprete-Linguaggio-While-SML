(* SINTASSI *)
(* Tipi base: Integer, Boolean, Skip *)

datatype tipiBase =  int | bool | unit
datatype ope = somma | maggug
type loc = string
datatype tipoloc = intref

datatype exp =
        Integer of int
    |	Boolean of bool
    |	Op of  exp * ope * exp
    |	If of exp * exp * exp
    |	Assign of loc * exp
    | 	Deref of loc
    | 	Skip
    | 	Seq of exp * exp
    | 	While of exp * exp

(* SEMANTICA *)
  (* Small Step *)
fun     isValue(Integer x) = true
    |   isValue(Boolean x) = true
    |   isValue(Skip) = true
    |   isValue(_) = false

fun     controllo([], l) = NONE
    |   controllo((l', n')::pairs, l) = 
            if l=l' then SOME n' 
                    else controllo(pairs, l)

fun     upsup testa [] (l,n) = NONE
    |   upsup testa ((l',n')::pairs) (l,n) =
            if l=l' then SOME(testa @ ((l,n)::pairs))
                    else upsup((l',n')::testa) pairs (l, n)

fun up((l,n), s) = upsup [] s (l,n)

fun     riduzione (Integer n, s) = NONE
    |   riduzione (Boolean n, s) = NONE
    |   riduzione (Op(e1, ope, e2), s) = (
            case (e1,ope,e2) of
                (Integer i1, somma, Integer i2) => SOME(Integer(i1+i2), s)
                |(Integer i1, maggug, Integer i2) => SOME(Boolean(i1>=i2), s)
                |(e1, ope, e2) => (
                    if(isValue e1) then (
                        case riduzione(e2,s) of
                            SOME(e2', s') => SOME(Op(e1,ope,e2'), s')
                            |NONE => NONE
                    )
                    else (
                        case riduzione(e1,s) of
                            SOME(e1', s') => SOME(Op(e1',ope,e2), s')
                            |NONE => NONE
                    )
                )
        )
    |   riduzione (If(e1, e2, e3), s) = (
            case e1 of
                Boolean(true) => SOME(e2, s)
                |Boolean(false) => SOME(e3, s)
                |_ => (
                    case riduzione(e1,s) of
                        SOME(e1', s') => SOME(If(e1', e2, e3), s')
                        |NONE => NONE
                )
        )
    |   riduzione (Skip, s) = NONE
    |   riduzione (Seq(e1,e2), s) = (
            case e1 of 
                Skip => SOME(e2, s)
                |_ => ( 
                    case riduzione(e1,s) of
                        SOME(e1', s') => SOME(Seq(e1',e2), s')
                        |NONE => NONE
                )
        )
    |   riduzione (While(e1,e2), s) = SOME(If(e1, Seq(e2, While(e1, e2)), Skip),s)
    |   riduzione (Deref(l), s) = (
            case controllo(s,l) of
                SOME x => SOME(Integer x, s)
                |NONE => NONE
        )
    |   riduzione (Assign(l,e), s) = (
            case e of
                Integer n => ( 
                    case up((l,n), s) of
                        SOME(s') => SOME(Skip, s')
                        |NONE => NONE
                )
                |_ => ( 
                    case riduzione(e,s) of
                        SOME(e', s') => SOME(Assign(l, e'), s')
                        |NONE => NONE 
                )
        )
    
  (* Big Step *)
fun bigs(e,s) = 
        case riduzione(e,s) of
            SOME(e',s') => bigs(e',s')
            |NONE => (e,s)


(* TYPE CHECKER *)
fun     typecheck gamma (Integer x) = SOME(int)
    |   typecheck gamma (Boolean b) = SOME(bool)
    |   typecheck gamma (Skip) = SOME(unit)
    |   typecheck gamma (Op(e1, ope, e2)) = (
            case (typecheck gamma e1, ope, typecheck gamma e2) of
                (SOME int, somma, SOME int) => SOME(int)
                |(SOME int, maggug, SOME int) => SOME(bool)
                |_ => NONE
        )
    |   typecheck gamma (While(e1, e2)) = (
            case (typecheck gamma e1, typecheck gamma e2) of
                (SOME bool, SOME unit) => SOME(unit)
                |_ => NONE
        )
    |   typecheck gamma (Assign(l,e)) = (
            case (controllo(gamma, l), typecheck gamma e) of
                (SOME intref, SOME int) => SOME(unit)
                |_ => NONE
        )
    |   typecheck gamma (Deref(l)) = (
            case (controllo(gamma, l)) of
                (SOME intref) => SOME(int)
                |_ => NONE
        )
    |   typecheck gamma (If(e1,e2,e3)) = (
            case (typecheck gamma e1, typecheck gamma e2, typecheck gamma e3) of
                (SOME bool, SOME t2, SOME t3) => ( if t2=t3 then SOME(t2) else NONE )
                |_ => NONE
        )
    |   typecheck gamma (Seq(e1, e2)) = (
            case (typecheck gamma e1, typecheck gamma e2) of
                (SOME unit, SOME t) => SOME(t)
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
    |   stampalw (Deref(l)) = "!" ^l
    |   stampalw (Assign(l, e)) = l ^ ":=" ^ stampalw e
    |   stampalw (Seq(e1,e2)) = stampalw e1 ^ "; " ^ stampalw e2