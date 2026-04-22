// Ejemplo 4 — INTERMEDIO-MEDIO: structs, composición, funciones que los manipulan.
// Cubre: type X struct, campos, struct literals posicionales y nominales,
// acceso a campos, funciones que toman y retornan structs.
package main

import "fmt"

type Punto struct {
	x int64
	y int64
}

type Rectangulo struct {
	esquinaInferior Punto
	ancho           int64
	alto            int64
}

type Producto struct {
	nombre   string
	precio   float64
	cantidad int64
}

// Funciones sobre Punto: toman y retornan structs nuevos (inmutable)
func desplazar(p Punto, dx int64, dy int64) Punto {
	return Punto{p.x + dx, p.y + dy}
}

func distanciaCuadrada(p1 Punto, p2 Punto) int64 {
	dx := p1.x - p2.x
	dy := p1.y - p2.y
	return dx*dx + dy*dy
}

func sumarPuntos(p1 Punto, p2 Punto) Punto {
	return Punto{p1.x + p2.x, p1.y + p2.y}
}

func esOrigen(p Punto) bool {
	if p.x == 0 && p.y == 0 {
		return true
	}
	return false
}

// Funciones sobre Rectangulo: composición de Punto
func area(r Rectangulo) int64 {
	return r.ancho * r.alto
}

func perimetro(r Rectangulo) int64 {
	return 2*r.ancho + 2*r.alto
}

func esquinaSuperior(r Rectangulo) Punto {
	return Punto{
		r.esquinaInferior.x + r.ancho,
		r.esquinaInferior.y + r.alto,
	}
}

// Clasificación basada en propiedades del struct
func clasificarRectangulo(r Rectangulo) string {
	if r.ancho == r.alto {
		return "cuadrado"
	}
	if r.ancho > r.alto {
		return "horizontal"
	}
	return "vertical"
}

// Funciones sobre Producto (usa literales nominales)
func precioTotal(p Producto) float64 {
	return p.precio * float64(p.cantidad)
}

func aplicarDescuento(p Producto, porcentaje float64) Producto {
	nuevoPrecio := p.precio * (1.0 - porcentaje/100.0)
	return Producto{
		nombre:   p.nombre,
		precio:   nuevoPrecio,
		cantidad: p.cantidad,
	}
}

func hayStock(p Producto) bool {
	if p.cantidad > 0 {
		return true
	}
	return false
}

// Función void sobre struct
func imprimirPunto(p Punto) {
	fmt.Printf("Punto(%d, %d)\n", p.x, p.y)
}

func imprimirProducto(p Producto) {
	fmt.Printf("  %s: $%.2f x %d = $%.2f\n",
		p.nombre, p.precio, p.cantidad, precioTotal(p))
}

func main() {
	fmt.Println("=== Puntos ===")
	p1 := Punto{3, 4}
	p2 := Punto{0, 0}
	p3 := desplazar(p1, 10, 10)

	imprimirPunto(p1)
	imprimirPunto(p2)
	imprimirPunto(p3)

	fmt.Printf("distancia² p1 a p2: %d\n", distanciaCuadrada(p1, p2))
	fmt.Printf("distancia² p1 a p3: %d\n", distanciaCuadrada(p1, p3))

	p4 := sumarPuntos(p1, Punto{5, 5})
	fmt.Print("suma p1 + (5,5): ")
	imprimirPunto(p4)

	fmt.Printf("p2 es origen: %t\n", esOrigen(p2))
	fmt.Printf("p1 es origen: %t\n", esOrigen(p1))

	fmt.Println("\n=== Rectángulos ===")
	r1 := Rectangulo{Punto{0, 0}, 10, 5}
	r2 := Rectangulo{Punto{2, 2}, 4, 4}
	r3 := Rectangulo{Punto{1, 1}, 3, 8}

	fmt.Printf("r1: ancho=%d alto=%d área=%d perímetro=%d forma=%s\n",
		r1.ancho, r1.alto, area(r1), perimetro(r1), clasificarRectangulo(r1))
	fmt.Printf("r2: ancho=%d alto=%d área=%d perímetro=%d forma=%s\n",
		r2.ancho, r2.alto, area(r2), perimetro(r2), clasificarRectangulo(r2))
	fmt.Printf("r3: ancho=%d alto=%d área=%d perímetro=%d forma=%s\n",
		r3.ancho, r3.alto, area(r3), perimetro(r3), clasificarRectangulo(r3))

	sup := esquinaSuperior(r1)
	fmt.Print("esquina superior de r1: ")
	imprimirPunto(sup)

	fmt.Println("\n=== Productos (literales nominales) ===")
	manzana := Producto{nombre: "Manzana", precio: 0.50, cantidad: 12}
	pan := Producto{nombre: "Pan", precio: 2.75, cantidad: 3}
	agotado := Producto{nombre: "Leche", precio: 1.80, cantidad: 0}

	imprimirProducto(manzana)
	imprimirProducto(pan)
	imprimirProducto(agotado)

	fmt.Printf("hay stock manzana: %t\n", hayStock(manzana))
	fmt.Printf("hay stock leche: %t\n", hayStock(agotado))

	fmt.Println("\n=== Aplicando descuento del 20%% ===")
	manzanaRebajada := aplicarDescuento(manzana, 20.0)
	imprimirProducto(manzanaRebajada)

	panRebajado := aplicarDescuento(pan, 20.0)
	imprimirProducto(panRebajado)
}
