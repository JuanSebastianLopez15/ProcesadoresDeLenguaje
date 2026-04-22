package main

import "fmt"

func esPalindromo(chars []rune, inicio int, fin int) bool {
	if inicio >= fin {
		return true
	}
	if chars[inicio] != chars[fin] {
		return false
	}
	return esPalindromo(chars, inicio+1, fin-1)
}

func main() {
	palabra := "oñoño"
	chars := []rune(palabra)
	fmt.Printf("Palabra: %s\n", palabra)
	fmt.Printf("Longitud en runas: %d\n", len(chars))
	
	if esPalindromo(chars, 0, len(chars)-1) {
		fmt.Println("Es palíndromo")
	} else {
		fmt.Println("No es palíndromo")
	}

	palabra2 := "ñandú"
	chars2 := []rune(palabra2)
	fmt.Printf("Palabra 2: %s (longitud runas: %d)\n", palabra2, len(chars2))
	
	// Convertir de vuelta a string
	s2 := string(chars2)
	fmt.Printf("String de vuelta: %s\n", s2)
}
