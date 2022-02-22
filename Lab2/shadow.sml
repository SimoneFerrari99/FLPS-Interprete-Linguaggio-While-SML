val x = 12;
AS | x:int
AD | x=12

val n = 2 + x;
AS | x:int, n:int
AD | x=12, n=14

val x = n - 14;
AS | x:int, n:int
AD | x=0, n=14

val n = n * x;
AS | x:int, n:int
AD | x=0, n=0

val b = if n = x then 8 else 5;
AS | x:int, n:int, b:int
AD | x=0, n=0, b=8

val a = if b = 5 then x else b;
AS | x:int, n:int, b:int, a:int
AD | x=0, n=0, b=8, a=8