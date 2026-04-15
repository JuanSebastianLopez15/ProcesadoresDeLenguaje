let calcularDoble (numero : int) : int =
(numero * 2)

let mostrarMensaje (texto : string) : unit =
print_endline ("Mensaje:" ^ " " ^ texto)

let main () : unit =
let contador = ref 1 in (
while (!contador <= 5) do
print_endline ("Iteración:" ^ " " ^ string_of_int !contador);
if ((!contador mod 2) = 0) then
mostrarMensaje "El número es par" else
mostrarMensaje "El número es impar";
let resultado = calcularDoble !contador in (
print_endline ("El doble es:" ^ " " ^ string_of_int resultado);
contador := !contador + 1)
   done)

let () = main ()