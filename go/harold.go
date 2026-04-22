package main

import "fmt"

type Producto struct {
	id       int64
	nombre   string
	precio   int64
	cantidad int64
	activo   bool
}

func calcularValorTotal(p Producto) int64 {
	if !p.activo {
		return 0
	}
	return p.precio * p.cantidad
}

func sumarInventario(productos []Producto, indice int, total int64) int64 {
	if indice == len(productos) {
		return total
	}
	valorProducto := calcularValorTotal(productos[indice])
	return sumarInventario(productos, indice+1, total+valorProducto)
}

func encontrarMasCaro(productos []Producto, indice int, masCaro Producto) Producto {
	if indice == len(productos) {
		return masCaro
	}
	actual := productos[indice]
	if actual.activo && actual.precio > masCaro.precio {
		return encontrarMasCaro(productos, indice+1, actual)
	}
	return encontrarMasCaro(productos, indice+1, masCaro)
}

func filtrarInactivos(productos []Producto, indice int, inactivos []Producto) []Producto {
	if indice == len(productos) {
		return inactivos
	}
	actual := productos[indice]
	if !actual.activo {
		inactivos = append(inactivos, actual)
	}
	return filtrarInactivos(productos, indice+1, inactivos)
}

func imprimirProductos(productos []Producto, indice int) {
	if indice == len(productos) {
		return
	}
	p := productos[indice]
	estado := "ACTIVO"
	if !p.activo {
		estado = "INACTIVO"
	}
	fmt.Printf("[%s] %s | Precio: $%d | Cantidad: %d\n", estado, p.nombre, p.precio, p.cantidad)
	imprimirProductos(productos, indice+1)
}

func main() {
	catalogo := []Producto{
		{id: 1, nombre: "Laptop", precio: 1200, cantidad: 5, activo: true},
		{id: 2, nombre: "Raton", precio: 25, cantidad: 50, activo: true},
		{id: 3, nombre: "Teclado", precio: 75, cantidad: 0, activo: false},
		{id: 4, nombre: "Monitor", precio: 300, cantidad: 10, activo: true},
		{id: 5, nombre: "Cable HDMI", precio: 15, cantidad: 100, activo: false},
	}

	fmt.Println("=== Catálogo Completo ===")
	imprimirProductos(catalogo, 0)

	valorTotal := sumarInventario(catalogo, 0, 0)
	fmt.Printf("\nValor total del inventario activo: $%d\n", valorTotal)

	if len(catalogo) > 0 {
		productoBase := catalogo[0]
		if !productoBase.activo {
			productoBase.precio = -1 // >:v
		}
		masCaro := encontrarMasCaro(catalogo, 0, productoBase)
		fmt.Printf("Producto activo más caro: %s ($%d)\n", masCaro.nombre, masCaro.precio)
	}
	productosInactivos := filtrarInactivos(catalogo, 0, []Producto{})
	fmt.Println("\n=== Productos Inactivos ===")
	imprimirProductos(productosInactivos, 0)
}