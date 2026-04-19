# ProcesadoresDeLenguajes

## Requisitos
- OPAM con switch activo.
- Dune.
- Menhir.
- OUnit2.

## Instalacion recomendada:
```powershell
opam install menhir ounit2
```
## Comandos utiles:
```powershell
opam exec -- dune build
opam exec -- dune runtest
opam exec -- dune exec ./test/e2e/run_sufrimiento_e2e.exe
opam exec -- dune exec ./test/e2e/test_go_files_e2e.exe
opam exec -- dune exec ./test/e2e/test_xtreme_files_e2e.exe
```

## Uso del traductor Go → OCaml:
El proyecto incluye un compilador que traduce un subconjunto de Go (similar al de los ejemplos) a código OCaml ejecutable.

## Compilar el traductor:
```powershell
dune build
```

## Traducir un archivo Go a OCaml:
```powershell
dune exec ./bin/main.exe "ruta/al/archivo_go.txt" "archivo_salida.ml"
```

Por ejemplo, para el archivo de ejemplo:
```powershell
dune exec ./bin/main.exe "Source Code/sufrimiento_go.txt" output.ml
```

Este comando:
- Realiza el análisis léxico y sintáctico.

- Ejecuta el type checker (middleware).

- Genera código OCaml válido en output.ml.

## Ejecutar el código OCaml generado:
Puedes ejecutarlo directamente con el intérprete ocaml:

```powershell
ocaml output.ml
```

## Notas sobre codificación:
El generador respeta caracteres UTF-8 (tildes, eñes) y los produce correctamente en el código OCaml. No es necesario normalizar a ASCII.

## Flujo actual del compilador:
El flujo implementado hoy conecta frontend, middleware y codegen con parser funcional para el subconjunto Go-like del proyecto.

1. Frontend - Lexer
- Archivo: frontend/lexer.mll
- Transforma código fuente Go-like en tokens (lib/token.ml).
- Reconoce selectores con punto (.) mediante el token DOT.

2. Frontend - Parser
- Archivo: frontend/parser.mly
- Parser Menhir que construye el AST (lib/ast.ml).
- Cubre package, import, func, if/else, for condicional, return, llamadas, selectores y expresiones básicas.

## AST compartido:
- Archivo: lib/ast.ml
- Define el AST rico usado por frontend, middleware y codegen:
	- tipos (typ), expresiones (expr), sentencias (stmt), declaraciones (decl), programa (program).

## Middleware - Typechecker:
- Archivos: middleware/env.ml, middleware/typecheck.ml
- Env mantiene bindings de variables/funciones y builtins.
- Typecheck valida tipos de expresiones, sentencias y el programa completo.

## Codegen - Generación de código OCaml:
- Archivo: codegen/codegen.ml
- Recorre el AST tipado y produce código OCaml sintácticamente correcto.
- Maneja variables mutables (las declara como ref y las desreferencia con !).
- Traduce fmt.Println a print_endline con concatenación de strings y conversiones de tipo apropiadas.
- Respeta los strings originales (UTF-8, tildes).

## E2E y pruebas:
- Demo por consola: test/e2e/run_sufrimiento_e2e.ml
	- Lee Source Code/sufrimiento_go.txt, imprime tokens, AST y resultado del typecheck.
- Pruebas E2E por fixtures: test/e2e/test_go_files_e2e.ml
	- Recorre archivos .txt en Source Code/tests.
	- Ejecuta pipeline completo lexer -> parser -> typecheck y compara con resultados esperados.
	- Imprime resumen por archivo con test/e2e/e2e_report.ml.
- Pruebas xtreme obligatorias: test/e2e/test_xtreme_files_e2e.ml
	- Recorre dinamicamente Source Code/xtreme_tests y ejecuta cada archivo completo.
	- Ejecuta pipeline completo lexer -> parser -> typecheck -> codegen por archivo.
	- Es criterio bloqueante: si un archivo xtreme falla, falla dune runtest.
	- Nuevos archivos .go/.txt en xtreme_tests se incorporan automaticamente en la siguiente corrida.

## Estado actual:
- Build y pruebas en verde con Dune.
- Lexer funcional para tokens base y DOT.
- Middleware de typecheck operativo sobre AST rico.
- Parser funcional para el subconjunto Go-like usado por tests y fixtures E2E.
- Codegen completo: genera código OCaml ejecutable y correcto para programas como sufrimiento_go.txt.
- El traductor end‑to‑end (Go → OCaml) funciona y está listo para su uso en la sustentación.

## Ejemplo completo:
# 1. Activar OPAM
```powershell
(& opam env) -split '\r?\n' | ForEach-Object { Invoke-Expression $_ }
```
# 2. Compilar el traductor
```powershell
dune build
```
# 3. Traducir el archivo Go de ejemplo
```powershell
dune exec ./bin/main.exe "Source Code/sufrimiento_go.txt" output.ml
```
# 4. Ejecutar el OCaml generado
```powershell
ocaml output.ml
```

## Salida esperada:
Iteración: 1
Mensaje: El número es impar
El doble es: 2
Iteración: 2
Mensaje: El número es par
El doble es: 4
Iteración: 3
Mensaje: El número es impar
El doble es: 6
Iteración: 4
Mensaje: El número es par
El doble es: 8
Iteración: 5
Mensaje: El número es impar
El doble es: 10