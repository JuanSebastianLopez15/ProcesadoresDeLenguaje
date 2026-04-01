# ProcesadoresDeLenguaje

## Requisitos
- OPAM con switch activo.
- Dune.
- Menhir.
- OUnit2.

Instalacion recomendada:

```powershell
opam install menhir ounit2
```

## Comandos utiles

```powershell
opam exec -- dune build
opam exec -- dune runtest
opam exec -- dune exec ./test/e2e/run_sufrimiento_e2e.exe
opam exec -- dune exec ./test/e2e/test_go_files_e2e.exe
```

## Flujo actual del compilador

El flujo implementado hoy conecta frontend y middleware con parser funcional para el subconjunto Go-like del proyecto.

1. Frontend - Lexer
- Archivo: `frontend/lexer.mll`
- Toma codigo fuente Go-like y lo transforma en tokens (`lib/token.ml`).
- Ya reconoce selectores con punto (`.`) mediante token `DOT`.

2. Frontend - Parser
- Archivo: `frontend/parser.mly`
- Parser Menhir que construye `program` (`lib/ast.ml`) desde fuente Go-like.
- Cubre `package`, `import`, `func`, `if/else`, `for` condicional, `return`, llamadas, selectores y expresiones basicas usadas en fixtures.

3. AST compartido
- Archivo: `lib/ast.ml`
- Define el AST rico que usan frontend/middleware:
	- tipos (`typ`), expresiones (`expr`), sentencias (`stmt`), declaraciones (`decl`), programa (`program`).

4. Middleware - Entorno y Typecheck
- Archivos: `middleware/env.ml`, `middleware/typecheck.ml`
- `Env` mantiene bindings de variables/funciones y builtins.
- `Typecheck` valida tipos de expresiones, sentencias y programa completo.

5. E2E y pruebas
- E2E demo por consola: `test/e2e/run_sufrimiento_e2e.ml`
	- Lee `Source Code/sufrimiento_go.txt`.
- Imprime fuente, tokens, AST parseado y resultado del typecheck.
- E2E por fixtures: `test/e2e/test_go_files_e2e.ml`
	- Recorre archivos `.txt` en `Source Code/tests`.
- Ejecuta pipeline completo `lexer -> parser -> typecheck` esperado por fixture.
	- Imprime resumen por archivo con `test/e2e/e2e_report.ml`.

## Estado actual
- Build y pruebas en verde con Dune.
- Lexer funcional para tokens base y `DOT`.
- Middleware de typecheck operativo sobre AST rico.
- Parser funcional para el subconjunto Go-like usado por tests y fixtures E2E.