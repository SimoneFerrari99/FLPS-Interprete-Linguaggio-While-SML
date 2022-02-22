(*Albero binario*)

datatype albero = 
  foglia of int
  | nodo of albero*int*albero;

val a = foglia;
val b = nodo;
val c = nodo(foglia(2), 1, foglia(4));

datatype 'a au =
  foglia of 'a
  | nodo of 'a au * 'a * 'a au;