// Ejemplo 6 — MEDIO-ALTO: nivel máximo del subset reducido.
// Cubre: múltiples structs interrelacionados, pipelines de transformación,
// pases múltiples, clasificación multi-criterio, recursión mutua sobre slices.
package main

import "fmt"

type Libro struct {
	titulo    string
	autor     string
	ano       int64
	paginas   int64
	categoria string
	rating    float64
}

type EstadisticaAutor struct {
	autor        string
	cantidad     int64
	promedioRating float64
	totalPaginas int64
}

// Validación básica
func esRatingValido(rating float64) bool {
	if rating < 0.0 || rating > 5.0 {
		return false
	}
	return true
}

func esLibroValido(l Libro) bool {
	if l.ano < 1000 || l.ano > 2030 {
		return false
	}
	if l.paginas <= 0 {
		return false
	}
	if !esRatingValido(l.rating) {
		return false
	}
	return true
}

// Clasificación
func clasificarPorLongitud(paginas int64) string {
	if paginas < 150 {
		return "corto"
	}
	if paginas < 400 {
		return "medio"
	}
	if paginas < 700 {
		return "largo"
	}
	return "épico"
}

func clasificarPorRating(rating float64) string {
	if rating < 2.0 {
		return "no recomendado"
	}
	if rating < 3.5 {
		return "regular"
	}
	if rating < 4.5 {
		return "bueno"
	}
	return "excelente"
}

// Filtros
func filtrarPorCategoria(libros []Libro, cat string, indice int, acc []Libro) []Libro {
	if indice == len(libros) {
		return acc
	}
	if libros[indice].categoria == cat {
		acc = append(acc, libros[indice])
	}
	return filtrarPorCategoria(libros, cat, indice+1, acc)
}

func filtrarPorAutor(libros []Libro, autor string, indice int, acc []Libro) []Libro {
	if indice == len(libros) {
		return acc
	}
	if libros[indice].autor == autor {
		acc = append(acc, libros[indice])
	}
	return filtrarPorAutor(libros, autor, indice+1, acc)
}

func filtrarPorRatingMinimo(libros []Libro, minimo float64, indice int, acc []Libro) []Libro {
	if indice == len(libros) {
		return acc
	}
	if libros[indice].rating >= minimo {
		acc = append(acc, libros[indice])
	}
	return filtrarPorRatingMinimo(libros, minimo, indice+1, acc)
}

func filtrarPorAno(libros []Libro, desde int64, hasta int64, indice int, acc []Libro) []Libro {
	if indice == len(libros) {
		return acc
	}
	ano := libros[indice].ano
	if ano >= desde && ano <= hasta {
		acc = append(acc, libros[indice])
	}
	return filtrarPorAno(libros, desde, hasta, indice+1, acc)
}

// Agregadores
func sumarPaginas(libros []Libro, indice int, acumulado int64) int64 {
	if indice == len(libros) {
		return acumulado
	}
	return sumarPaginas(libros, indice+1, acumulado+libros[indice].paginas)
}

func sumarRatings(libros []Libro, indice int, acumulado float64) float64 {
	if indice == len(libros) {
		return acumulado
	}
	return sumarRatings(libros, indice+1, acumulado+libros[indice].rating)
}

func contarLibros(libros []Libro, indice int, contador int64) int64 {
	if indice == len(libros) {
		return contador
	}
	return contarLibros(libros, indice+1, contador+1)
}

// Búsqueda
func libroMasLargo(libros []Libro, indice int, mejorIndice int) int {
	if indice == len(libros) {
		return mejorIndice
	}
	if libros[indice].paginas > libros[mejorIndice].paginas {
		return libroMasLargo(libros, indice+1, indice)
	}
	return libroMasLargo(libros, indice+1, mejorIndice)
}

func libroMejorRating(libros []Libro, indice int, mejorIndice int) int {
	if indice == len(libros) {
		return mejorIndice
	}
	if libros[indice].rating > libros[mejorIndice].rating {
		return libroMejorRating(libros, indice+1, indice)
	}
	return libroMejorRating(libros, indice+1, mejorIndice)
}

// Validación recursiva: todos los libros del catálogo son válidos
func todosValidos(libros []Libro, indice int) bool {
	if indice == len(libros) {
		return true
	}
	if !esLibroValido(libros[indice]) {
		return false
	}
	return todosValidos(libros, indice+1)
}

// Procesamiento compuesto: construir estadística por autor
func estadisticasDeAutor(libros []Libro, autor string) EstadisticaAutor {
	delAutor := filtrarPorAutor(libros, autor, 0, []Libro{})
	cantidad := contarLibros(delAutor, 0, 0)
	paginas := sumarPaginas(delAutor, 0, 0)
	ratingSuma := sumarRatings(delAutor, 0, 0.0)
	promedio := 0.0
	if cantidad > 0 {
		promedio = ratingSuma / float64(cantidad)
	}
	return EstadisticaAutor{
		autor:          autor,
		cantidad:       cantidad,
		promedioRating: promedio,
		totalPaginas:   paginas,
	}
}

// Verificar si un autor publicó en un rango de años
func autorPublicoEn(libros []Libro, autor string, desde int64, hasta int64, indice int) bool {
	if indice == len(libros) {
		return false
	}
	l := libros[indice]
	if l.autor == autor && l.ano >= desde && l.ano <= hasta {
		return true
	}
	return autorPublicoEn(libros, autor, desde, hasta, indice+1)
}

// Impresión
func imprimirLibro(l Libro) {
	longitud := clasificarPorLongitud(l.paginas)
	calidad := clasificarPorRating(l.rating)
	fmt.Printf("  \"%s\" por %s (%d) — %d págs [%s], rating %.1f [%s], cat: %s\n",
		l.titulo, l.autor, l.ano, l.paginas, longitud, l.rating, calidad, l.categoria)
}

func imprimirCatalogo(libros []Libro, indice int) {
	if indice == len(libros) {
		return
	}
	imprimirLibro(libros[indice])
	imprimirCatalogo(libros, indice+1)
}

func imprimirEstadistica(e EstadisticaAutor) {
	if e.cantidad == 0 {
		fmt.Printf("  %s: sin libros registrados\n", e.autor)
		return
	}
	promPags := e.totalPaginas / e.cantidad
	fmt.Printf("  %s: %d libros, %d págs totales (%d prom), rating prom %.2f\n",
		e.autor, e.cantidad, e.totalPaginas, promPags, e.promedioRating)
}

func main() {
	catalogo := []Libro{
		{"1984", "Orwell", 1949, 328, "ficción", 4.5},
		{"Animal Farm", "Orwell", 1945, 112, "ficción", 4.2},
		{"Sapiens", "Harari", 2011, 443, "historia", 4.6},
		{"Homo Deus", "Harari", 2016, 464, "historia", 4.1},
		{"Cosmos", "Sagan", 1980, 384, "ciencia", 4.7},
		{"Pale Blue Dot", "Sagan", 1994, 429, "ciencia", 4.4},
		{"Dune", "Herbert", 1965, 688, "ficción", 4.8},
		{"Foundation", "Asimov", 1951, 255, "ficción", 4.3},
	}

	fmt.Println("=== Catálogo completo ===")
	imprimirCatalogo(catalogo, 0)

	fmt.Println("\n=== Validación ===")
	fmt.Printf("todos los libros son válidos: %t\n", todosValidos(catalogo, 0))

	fmt.Println("\n=== Estadísticas generales ===")
	totalLibros := contarLibros(catalogo, 0, 0)
	totalPags := sumarPaginas(catalogo, 0, 0)
	fmt.Printf("total libros: %d\n", totalLibros)
	fmt.Printf("total páginas: %d\n", totalPags)

	idxLargo := libroMasLargo(catalogo, 1, 0)
	fmt.Printf("libro más largo: %s (%d págs)\n",
		catalogo[idxLargo].titulo, catalogo[idxLargo].paginas)

	idxMejor := libroMejorRating(catalogo, 1, 0)
	fmt.Printf("libro mejor calificado: %s (%.1f)\n",
		catalogo[idxMejor].titulo, catalogo[idxMejor].rating)

	fmt.Println("\n=== Por categoría ===")
	fmt.Println("Ficción:")
	imprimirCatalogo(filtrarPorCategoria(catalogo, "ficción", 0, []Libro{}), 0)

	fmt.Println("Ciencia:")
	imprimirCatalogo(filtrarPorCategoria(catalogo, "ciencia", 0, []Libro{}), 0)

	fmt.Println("\n=== Rating >= 4.5 ===")
	excelentes := filtrarPorRatingMinimo(catalogo, 4.5, 0, []Libro{})
	imprimirCatalogo(excelentes, 0)

	fmt.Println("\n=== Publicados 1980-2000 ===")
	eraMedia := filtrarPorAno(catalogo, 1980, 2000, 0, []Libro{})
	imprimirCatalogo(eraMedia, 0)

	fmt.Println("\n=== Estadísticas por autor ===")
	imprimirEstadistica(estadisticasDeAutor(catalogo, "Orwell"))
	imprimirEstadistica(estadisticasDeAutor(catalogo, "Sagan"))
	imprimirEstadistica(estadisticasDeAutor(catalogo, "Harari"))
	imprimirEstadistica(estadisticasDeAutor(catalogo, "Herbert"))

	fmt.Println("\n=== Consultas específicas ===")
	fmt.Printf("¿Sagan publicó entre 1990-2000? %t\n",
		autorPublicoEn(catalogo, "Sagan", 1990, 2000, 0))
	fmt.Printf("¿Orwell publicó entre 1950-1960? %t\n",
		autorPublicoEn(catalogo, "Orwell", 1950, 1960, 0))
}
