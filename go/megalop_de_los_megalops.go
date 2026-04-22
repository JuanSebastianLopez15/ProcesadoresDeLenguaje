package main

import "fmt"

func collatz(n int64, pasos int64) int64 {
	if n == 1 {
		return pasos
	}
	if n%2 == 0 {
		return collatz(n/2, pasos+1)
	}
	return collatz((n*3)+1, pasos+1)
}

func mcd(a int64, b int64) int64 {
	if b == 0 {
		return a
	}
	return mcd(b, a%b)
}

func clasificarCollatz(pasos int64) string {
	if pasos < 10 {
		return "Rapido"
	}
	if pasos >= 10 && pasos <= 50 {
		return "Normal"
	}
	return "Largo"
}

func procesarPares(a int64, b int64) {
	maximoComunDivisor := mcd(a, b)
	pasosA := collatz(a, 0)
	pasosB := collatz(b, 0)

	fmt.Printf("Números: %d y %d | MCD: %d\n", a, b, maximoComunDivisor)
	fmt.Printf("Collatz %d: %d pasos (%s)\n", a, pasosA, clasificarCollatz(pasosA))
	fmt.Printf("Collatz %d: %d pasos (%s)\n", b, pasosB, clasificarCollatz(pasosB))
}

func main() {
	fmt.Println("=== Analizador Matemático ===")
	procesarPares(48, 18)
	fmt.Println("-------------------")
	procesarPares(27, 82)
	fmt.Println("-------------------")
	procesarPares(7, 13)
}
