// Ejemplo 2 — INTERMEDIO-BAJO: guards múltiples, recursión mutua, lógicos.
// Cubre: múltiples if seguidos, &&, ||, !, recursión cruzada entre funciones.
package main

import "fmt"

// Guards múltiples: cada if es un caso separado
func clasificarTemperatura(grados int64) string {
	if grados < 0 {
		return "congelante"
	}
	if grados < 10 {
		return "frío"
	}
	if grados < 20 {
		return "templado"
	}
	if grados < 30 {
		return "cálido"
	}
	return "caliente"
}

func validarEdad(edad int64) bool {
	if edad < 0 {
		return false
	}
	if edad > 150 {
		return false
	}
	return true
}

func estaEnRango(valor int64, min int64, max int64) bool {
	if valor < min || valor > max {
		return false
	}
	return true
}

// Combinación de lógicos
func esAnoBisiesto(ano int64) bool {
	if ano%400 == 0 {
		return true
	}
	if ano%100 == 0 {
		return false
	}
	if ano%4 == 0 {
		return true
	}
	return false
}

// Recursión mutua (mapea a let rec ... and ... en OCaml)
func esParRecursivo(n int64) bool {
	if n == 0 {
		return true
	}
	if n < 0 {
		return esParRecursivo(-n)
	}
	return esImparRecursivo(n - 1)
}

func esImparRecursivo(n int64) bool {
	if n == 0 {
		return false
	}
	if n < 0 {
		return esImparRecursivo(-n)
	}
	return esParRecursivo(n - 1)
}

// Recursión con múltiples casos base
func fibonacci(n int64) int64 {
	if n < 0 {
		return 0
	}
	if n == 0 {
		return 0
	}
	if n == 1 {
		return 1
	}
	return fibonacci(n-1) + fibonacci(n-2)
}

// Contador con condición compuesta
func contarDigitos(numero int64) int64 {
	if numero < 0 {
		return contarDigitos(-numero)
	}
	if numero < 10 {
		return 1
	}
	return 1 + contarDigitos(numero/10)
}

func main() {
	fmt.Println("=== Clasificación ===")
	fmt.Printf("-5°C: %s\n", clasificarTemperatura(-5))
	fmt.Printf("15°C: %s\n", clasificarTemperatura(15))
	fmt.Printf("25°C: %s\n", clasificarTemperatura(25))
	fmt.Printf("35°C: %s\n", clasificarTemperatura(35))

	fmt.Println("\n=== Validación ===")
	fmt.Printf("edad 30 válida: %t\n", validarEdad(30))
	fmt.Printf("edad -1 válida: %t\n", validarEdad(-1))
	fmt.Printf("50 en [10, 100]: %t\n", estaEnRango(50, 10, 100))
	fmt.Printf("150 en [10, 100]: %t\n", estaEnRango(150, 10, 100))

	fmt.Println("\n=== Años bisiestos ===")
	fmt.Printf("2000 bisiesto: %t\n", esAnoBisiesto(2000))
	fmt.Printf("1900 bisiesto: %t\n", esAnoBisiesto(1900))
	fmt.Printf("2024 bisiesto: %t\n", esAnoBisiesto(2024))
	fmt.Printf("2023 bisiesto: %t\n", esAnoBisiesto(2023))

	fmt.Println("\n=== Recursión mutua ===")
	fmt.Printf("10 es par: %t\n", esParRecursivo(10))
	fmt.Printf("7 es par: %t\n", esParRecursivo(7))
	fmt.Printf("7 es impar: %t\n", esImparRecursivo(7))

	fmt.Println("\n=== Fibonacci ===")
	fmt.Printf("fib(0) = %d\n", fibonacci(0))
	fmt.Printf("fib(5) = %d\n", fibonacci(5))
	fmt.Printf("fib(10) = %d\n", fibonacci(10))

	fmt.Println("\n=== Dígitos ===")
	fmt.Printf("dígitos 42: %d\n", contarDigitos(42))
	fmt.Printf("dígitos 1234567: %d\n", contarDigitos(1234567))
	fmt.Printf("dígitos -99: %d\n", contarDigitos(-99))
}
