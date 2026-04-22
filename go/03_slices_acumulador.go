// Ejemplo 3 — INTERMEDIO: slices con recursión acumulativa.
// Cubre: []int64, indexación, len, append, recursión tail-style,
// construcción de slices por recursión, reasignación lineal (shadowing).
package main

import "fmt"

// Suma de elementos con acumulador (tail-recursive en intención)
func sumar(lista []int64, indice int, acumulado int64) int64 {
	if indice == len(lista) {
		return acumulado
	}
	return sumar(lista, indice+1, acumulado+lista[indice])
}

// Producto con acumulador
func multiplicar(lista []int64, indice int, acumulado int64) int64 {
	if indice == len(lista) {
		return acumulado
	}
	return multiplicar(lista, indice+1, acumulado*lista[indice])
}

// Búsqueda del máximo
func maximo(lista []int64, indice int, actual int64) int64 {
	if indice == len(lista) {
		return actual
	}
	if lista[indice] > actual {
		return maximo(lista, indice+1, lista[indice])
	}
	return maximo(lista, indice+1, actual)
}

// Búsqueda del mínimo
func minimo(lista []int64, indice int, actual int64) int64 {
	if indice == len(lista) {
		return actual
	}
	if lista[indice] < actual {
		return minimo(lista, indice+1, lista[indice])
	}
	return minimo(lista, indice+1, actual)
}

// Conteo condicional
func contarPositivos(lista []int64, indice int, contador int64) int64 {
	if indice == len(lista) {
		return contador
	}
	if lista[indice] > 0 {
		return contarPositivos(lista, indice+1, contador+1)
	}
	return contarPositivos(lista, indice+1, contador)
}

// Búsqueda lineal: primer valor que cumple condición
func primerMayorQue(lista []int64, limite int64, indice int) int64 {
	if indice == len(lista) {
		return -1
	}
	if lista[indice] > limite {
		return lista[indice]
	}
	return primerMayorQue(lista, limite, indice+1)
}

// Verifica que todos los elementos cumplen una condición
func todosPositivos(lista []int64, indice int) bool {
	if indice == len(lista) {
		return true
	}
	if lista[indice] <= 0 {
		return false
	}
	return todosPositivos(lista, indice+1)
}

// Construcción de slice nuevo con transformación
func duplicarTodos(lista []int64, indice int, acumulado []int64) []int64 {
	if indice == len(lista) {
		return acumulado
	}
	acumulado = append(acumulado, lista[indice]*2)
	return duplicarTodos(lista, indice+1, acumulado)
}

// Filtrado con construcción de slice
func filtrarPares(lista []int64, indice int, acumulado []int64) []int64 {
	if indice == len(lista) {
		return acumulado
	}
	if lista[indice]%2 == 0 {
		acumulado = append(acumulado, lista[indice])
	}
	return filtrarPares(lista, indice+1, acumulado)
}

// Filtrado por rango con múltiples parámetros
func filtrarEnRango(lista []int64, min int64, max int64, indice int, acumulado []int64) []int64 {
	if indice == len(lista) {
		return acumulado
	}
	valor := lista[indice]
	if valor >= min && valor <= max {
		acumulado = append(acumulado, valor)
	}
	return filtrarEnRango(lista, min, max, indice+1, acumulado)
}

// Impresión recursiva con índice
func imprimirLista(lista []int64, indice int) {
	if indice == len(lista) {
		return
	}
	fmt.Printf("  [%d] = %d\n", indice, lista[indice])
	imprimirLista(lista, indice+1)
}

func main() {
	numeros := []int64{5, -2, 8, 3, -7, 10, 1, -4, 9, 6}

	fmt.Println("=== Lista original ===")
	imprimirLista(numeros, 0)

	fmt.Println("\n=== Agregados ===")
	fmt.Printf("suma: %d\n", sumar(numeros, 0, 0))
	fmt.Printf("máximo: %d\n", maximo(numeros, 1, numeros[0]))
	fmt.Printf("mínimo: %d\n", minimo(numeros, 1, numeros[0]))
	fmt.Printf("positivos: %d\n", contarPositivos(numeros, 0, 0))

	positivos := []int64{1, 2, 3, 4, 5}
	fmt.Printf("producto de [1..5]: %d\n", multiplicar(positivos, 0, 1))

	fmt.Println("\n=== Búsqueda ===")
	fmt.Printf("primer > 5: %d\n", primerMayorQue(numeros, 5, 0))
	fmt.Printf("primer > 100: %d\n", primerMayorQue(numeros, 100, 0))
	fmt.Printf("todos positivos en original: %t\n", todosPositivos(numeros, 0))
	fmt.Printf("todos positivos en [1..5]: %t\n", todosPositivos(positivos, 0))

	fmt.Println("\n=== Transformación ===")
	dobles := duplicarTodos(positivos, 0, []int64{})
	fmt.Println("duplicados de [1..5]:")
	imprimirLista(dobles, 0)

	pares := filtrarPares(numeros, 0, []int64{})
	fmt.Println("solo pares:")
	imprimirLista(pares, 0)

	enRango := filtrarEnRango(numeros, 1, 8, 0, []int64{})
	fmt.Println("en [1, 8]:")
	imprimirLista(enRango, 0)
}
