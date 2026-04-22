// Ejemplo 1 — BÁSICO: funciones puras con recursión simple.
// Cubre: parámetros tipados, retorno, aritmética, if/else, recursión directa.
package main

import "fmt"

func factorial(n int64) int64 {
	if n <= 1 {
		return 1
	}
	return n * factorial(n-1)
}

func potencia(base int64, exponente int64) int64 {
	if exponente == 0 {
		return 1
	}
	return base * potencia(base, exponente-1)
}

func suma(a int64, b int64) int64 {
	return a + b
}

func maximo(a int64, b int64) int64 {
	if a > b {
		return a
	}
	return b
}

func absoluto(n int64) int64 {
	if n < 0 {
		return -n
	}
	return n
}

func esMultiploDe(numero int64, divisor int64) bool {
	if numero%divisor == 0 {
		return true
	}
	return false
}

func main() {
	fmt.Printf("factorial(6) = %d\n", factorial(6))
	fmt.Printf("potencia(3, 4) = %d\n", potencia(3, 4))
	fmt.Printf("suma(15, 27) = %d\n", suma(15, 27))
	fmt.Printf("maximo(42, 17) = %d\n", maximo(42, 17))
	fmt.Printf("absoluto(-99) = %d\n", absoluto(-99))
	fmt.Printf("esMultiploDe(100, 7): %t\n", esMultiploDe(100, 7))
	fmt.Printf("esMultiploDe(100, 5): %t\n", esMultiploDe(100, 5))
}
