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

El flujo implementado hoy llega desde frontend hasta middleware con el parser aun en modo minimo.

1. Frontend - Lexer
- Archivo: `frontend/lexer.mll`
- Toma codigo fuente Go-like y lo transforma en tokens (`lib/token.ml`).
- Ya reconoce selectores con punto (`.`) mediante token `DOT`.

2. Frontend - Parser
- Archivo: `frontend/parser.mly`
- Estado actual: parser dummy (entrada minima para pruebas de infraestructura).
- Por eso, en la e2e principal se construye un AST canonico manualmente.

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
	- Imprime fuente, tokens, AST y resultado del typecheck.
- E2E por fixtures: `test/e2e/test_go_files_e2e.ml`
	- Recorre archivos `.txt` en `Source Code/tests`.
	- Ejecuta lexer + typecheck esperado por fixture.
	- Imprime resumen por archivo con `test/e2e/e2e_report.ml`.

## Estado actual
- Build y pruebas en verde con Dune.
- Lexer funcional para tokens base y `DOT`.
- Middleware de typecheck operativo sobre AST rico.
- Parser pendiente de evolucionar desde modo dummy a parser completo.