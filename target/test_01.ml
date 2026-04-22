let rec factorial (n : int) : int =
  if n <= 1 then
    1
  else
    n * factorial (n - 1)

let rec potencia (base : int) (exponente : int) : int =
  if exponente = 0 then
    1
  else
    base * potencia base (exponente - 1)

let suma (a : int) (b : int) : int =
  a + b

let maximo (a : int) (b : int) : int =
  if a > b then
    a
  else
    b

let absoluto (n : int) : int =
  if n < 0 then
    - (n)
  else
    n

let es_multiplo_de (numero : int) (divisor : int) : bool =
  numero mod divisor = 0

let main () : unit =
  Printf.printf "factorial(6) = %d\n" (factorial 6);
  Printf.printf "potencia(3, 4) = %d\n" (potencia 3 4);
  Printf.printf "suma(15, 27) = %d\n" (suma 15 27);
  Printf.printf "maximo(42, 17) = %d\n" (maximo 42 17);
  Printf.printf "absoluto(-99) = %d\n" (absoluto -99);
  Printf.printf "esMultiploDe(100, 7): %b\n" (es_multiplo_de 100 7);
  Printf.printf "esMultiploDe(100, 5): %b\n" (es_multiplo_de 100 5)

let () = main ()
