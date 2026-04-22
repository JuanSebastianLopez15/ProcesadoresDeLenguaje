package main

import "fmt"

func esPrimo(numero int64, divisor int64) bool {
	if numero < 2 {
		return false
	}
	if divisor*divisor > numero {
		return true
	}
	if numero%divisor == 0 {
		return false
	}
	return esPrimo(numero, divisor+1)
}

func calcularFibonacci(n int64) int64 {
	if n <= 0 {
		return 0
	}
	if n == 1 {
		return 1
	}
	return calcularFibonacci(n-1) + calcularFibonacci(n-2)
}

func sumarLista(lista []int64, indice int, acumulado int64) int64 {
	if indice == len(lista) {
		return acumulado
	}
	return sumarLista(lista, indice+1, acumulado+lista[indice])
}

func maximoLista(lista []int64, indice int, maximoActual int64) int64 {
	if indice == len(lista) {
		return maximoActual
	}
	valor := lista[indice]
	if valor > maximoActual {
		return maximoLista(lista, indice+1, valor)
	}
	return maximoLista(lista, indice+1, maximoActual)
}

func minimoLista(lista []int64, indice int, minimoActual int64) int64 {
	if indice == len(lista) {
		return minimoActual
	}
	valor := lista[indice]
	if valor < minimoActual {
		return minimoLista(lista, indice+1, valor)
	}
	return minimoLista(lista, indice+1, minimoActual)
}

func validarRango(valor int64, minimo int64, maximo int64) bool {
	return valor >= minimo && valor <= maximo
}

func validarListaNoVacia(lista []int64) bool {
	return len(lista) > 0
}

func validarTexto(texto string) bool {
	return len(texto) > 0
}

func clasificarNumero(numero int64) string {
	if !validarRango(numero, 1, 10000) {
		return "fuera de rango"
	}
	if esPrimo(numero, 2) {
		return "primo"
	}
	if numero%3 == 0 && numero%5 == 0 {
		return "multiplo de 3 y 5"
	}
	if numero%3 == 0 {
		return "multiplo de 3"
	}
	if numero%5 == 0 {
		return "multiplo de 5"
	}
	if numero%2 == 0 {
		return "par compuesto"
	}
	return "impar compuesto"
}

func calcularDigitos(numero int64, acumulado int64) int64 {
	if numero == 0 {
		return acumulado
	}
	return calcularDigitos(numero/10, acumulado+1)
}

func sumaDigitos(numero int64, acumulado int64) int64 {
	if numero == 0 {
		return acumulado
	}
	return sumaDigitos(numero/10, acumulado+(numero%10))
}

func esPalindromo(chars []rune, inicio int, fin int) bool {
	if inicio >= fin {
		return true
	}
	if chars[inicio] != chars[fin] {
		return false
	}
	return esPalindromo(chars, inicio+1, fin-1)
}

type Resultado struct {
	numero        int64
	clasificacion string
	fibonacci     int64
	digitos       int64
	sumaD         int64
}

func procesarNumero(numero int64) Resultado {
	clasificacion := clasificarNumero(numero)
	fibonacci := int64(-1)
	if numero <= 15 {
		fibonacci = calcularFibonacci(numero)
	}
	digitos := int64(1)
	if numero > 0 {
		digitos = calcularDigitos(numero, 0)
	}
	sumaD := sumaDigitos(numero, 0)
	return Resultado{numero, clasificacion, fibonacci, digitos, sumaD}
}

func procesarLista(lista []int64, indice int, resultados []Resultado) []Resultado {
	if indice == len(lista) {
		return resultados
	}
	valor := lista[indice]
	if validarRango(valor, 1, 10000) {
		resultados = append(resultados, procesarNumero(valor))
	}
	return procesarLista(lista, indice+1, resultados)
}

func imprimirResultado(r Resultado) {
	fmt.Printf("Número: %d | Tipo: %s | Fibonacci: %d | Dígitos: %d | Suma dígitos: %d\n",
		r.numero, r.clasificacion, r.fibonacci, r.digitos, r.sumaD)
}

func imprimirResultados(resultados []Resultado, indice int) {
	if indice == len(resultados) {
		return
	}
	imprimirResultado(resultados[indice])
	imprimirResultados(resultados, indice+1)
}

func contarPorTipo(resultados []Resultado, tipo string, indice int, contador int64) int64 {
	if indice == len(resultados) {
		return contador
	}
	if resultados[indice].clasificacion == tipo {
		return contarPorTipo(resultados, tipo, indice+1, contador+1)
	}
	return contarPorTipo(resultados, tipo, indice+1, contador)
}

func extraerFibonacci(resultados []Resultado, indice int, acumulado []int64) []int64 {
	if indice == len(resultados) {
		return acumulado
	}
	valor := resultados[indice].fibonacci
	if validarRango(valor, 0, 999999999) {
		acumulado = append(acumulado, valor)
	}
	return extraerFibonacci(resultados, indice+1, acumulado)
}

func generarReporte(resultados []Resultado, tipos []string, indice int) {
	if indice == len(tipos) {
		return
	}
	tipo := tipos[indice]
	total := contarPorTipo(resultados, tipo, 0, 0)
	fmt.Printf("Tipo: %s | Total: %d\n", tipo, total)
	generarReporte(resultados, tipos, indice+1)
}

func verificarPalindromos(palabras []string, indice int) {
	if indice == len(palabras) {
		return
	}
	palabra := palabras[indice]
	if !validarTexto(palabra) {
		verificarPalindromos(palabras, indice+1)
		return
	}
	chars := []rune(palabra)
	resultado := esPalindromo(chars, 0, len(chars)-1)
	if resultado {
		fmt.Printf("'%s' ES palindromo\n", palabra)
	} else {
		fmt.Printf("'%s' NO es palindromo\n", palabra)
	}
	verificarPalindromos(palabras, indice+1)
}

func iterar(contador int64, limite int64) {
	if contador > limite {
		return
	}
	if contador%5 == 0 {
		fmt.Printf("Bloque %d de %d procesado\n", contador, limite)
	}
	iterar(contador+1, limite)
}

func main() {
	numeros := []int64{2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20}
	palabras := []string{"ana", "python", "radar", "elixir", "nivel", "racket", "oso"}
	tipos := []string{"primo", "par compuesto", "impar compuesto", "multiplo de 3", "multiplo de 5", "multiplo de 3 y 5"}

	if !validarListaNoVacia(numeros) {
		fmt.Println("[ERROR] La lista de números esta vacia.")
		return
	}

	fmt.Println("=== Procesando números ===")
	resultados := procesarLista(numeros, 0, []Resultado{})
	imprimirResultados(resultados, 0)

	fibonaccis := extraerFibonacci(resultados, 0, []int64{})

	sumaFibonaccis := sumarLista(fibonaccis, 0, 0)
	maxFibonacci := int64(-1)
	minFibonacci := int64(-1)
	if validarListaNoVacia(fibonaccis) {
		maxFibonacci = maximoLista(fibonaccis, 0, fibonaccis[0])
		minFibonacci = minimoLista(fibonaccis, 0, fibonaccis[0])
	}

	fmt.Println("=== Estadísticas ===")
	fmt.Printf("Suma de fibonacci validos: %d\n", sumaFibonaccis)
	fmt.Printf("Mayor fibonacci: %d\n", maxFibonacci)
	fmt.Printf("Menor fibonacci: %d\n", minFibonacci)

	fmt.Println("=== Reporte por tipo ===")
	generarReporte(resultados, tipos, 0)

	fmt.Println("=== Verificacion de palindromos ===")
	verificarPalindromos(palabras, 0)

	fmt.Println("=== Iteracion con while ===")
	iterar(1, int64(len(numeros)))

	fmt.Println("Hola Mundo Felicitaciones has traducido el archivo!")
}
