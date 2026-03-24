#Cris es gay 

opam install menhir -> para el parser (gramatica) -> obligatorio instalar
opam install ounit2 -> libreria para correr las pruebas unitarias del test_lexer -> obligatorio instalar
dune runtest -> porbar los archivos dune estan bien configurados, ejecuta el codigo y verifica que el lexer funciona

# Estado actual del trabajo (si me falto algo me dicen)
- Sistema de compilación (Dune) configurado y enlazado.
- El analizador léxico ya reconoce los Tokens básicos del código fuente.
- Las pruebas pasan correctamente 

NOTA IMPORTANTE: esos archivos del build se crearon al ejecutar el comando dune runtest, no los hice yo.