(* ESEMPI COMPOSIZIONE PARALLELA *)
3 || 1
l:=1 || m:=5
if true then l:=1 else m:=2 || k:=2

(* ESEMPI SCELTA NON DETERMINISTICA *)
3 (+) 1
m:=5 (+) (skip)
x:=0; while true do x:=!x+1 (+) x:=1; z:=!x+2

(* ESEMPI AWAIT *)
await true protect 5 end
await true protect l:=1; m:=!l+5 end || l:=2
await !x>=3 protect l:=x end || x:=3