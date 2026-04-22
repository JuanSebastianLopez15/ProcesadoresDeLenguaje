package main

import "fmt"

func calcularAreaRectangulo(base int64, altura int64) int64 {
	return base * altura
}

func calcularPerimetro(base int64, altura int64) int64 {
	return (base * 2) + (altura * 2)
}

func esCuadrado(base int64, altura int64) bool {
	return base == altura
}

func evaluarDimensiones(base int64, altura int64) {
	area := calcularAreaRectangulo(base, altura)
	perimetro := calcularPerimetro(base, altura)
	cuadrado := esCuadrado(base, altura)

	fmt.Printf("Base: %d | Altura: %d\n", base, altura)
	fmt.Printf("Area: %d | Perimetro: %d\n", area, perimetro)

	if cuadrado {
		fmt.Println("La figura ES un cuadrado perfecto.")
	} else {
		fmt.Println("La figura NO es un cuadrado.")
	}
}

func main() {
	b1 := int64(10)
	a1 := int64(5)

	b2 := int64(8)
	a2 := int64(8)

	fmt.Println("=== Prueba 1 ===")
	evaluarDimensiones(b1, a1)

	fmt.Println("=== Prueba 2 ===")
	evaluarDimensiones(b2, a2)
}
