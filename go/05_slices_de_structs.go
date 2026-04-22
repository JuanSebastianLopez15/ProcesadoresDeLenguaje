// Ejemplo 5 — MEDIO: slices de structs, procesamiento recursivo,
// pipeline de transformaciones. Similar al nivel del archivo de Resultado.
// Cubre: []Struct, literales anidados, múltiples pases de procesamiento.
package main

import "fmt"

type Empleado struct {
	nombre         string
	edad           int64
	salario        float64
	departamento   string
	anosExperiencia int64
}

type Resumen struct {
	departamento string
	totalEmpleados int64
	salarioTotal   float64
}

// Filtros: construyen slices nuevos
func filtrarPorDepartamento(empleados []Empleado, depto string, indice int, acc []Empleado) []Empleado {
	if indice == len(empleados) {
		return acc
	}
	if empleados[indice].departamento == depto {
		acc = append(acc, empleados[indice])
	}
	return filtrarPorDepartamento(empleados, depto, indice+1, acc)
}

func filtrarPorSalarioMinimo(empleados []Empleado, minimo float64, indice int, acc []Empleado) []Empleado {
	if indice == len(empleados) {
		return acc
	}
	if empleados[indice].salario >= minimo {
		acc = append(acc, empleados[indice])
	}
	return filtrarPorSalarioMinimo(empleados, minimo, indice+1, acc)
}

func filtrarSeniors(empleados []Empleado, anosMinimos int64, indice int, acc []Empleado) []Empleado {
	if indice == len(empleados) {
		return acc
	}
	if empleados[indice].anosExperiencia >= anosMinimos {
		acc = append(acc, empleados[indice])
	}
	return filtrarSeniors(empleados, anosMinimos, indice+1, acc)
}

// Agregadores recursivos
func sumarSalarios(empleados []Empleado, indice int, acumulado float64) float64 {
	if indice == len(empleados) {
		return acumulado
	}
	return sumarSalarios(empleados, indice+1, acumulado+empleados[indice].salario)
}

func contarEmpleados(empleados []Empleado, indice int, contador int64) int64 {
	if indice == len(empleados) {
		return contador
	}
	return contarEmpleados(empleados, indice+1, contador+1)
}

func promedioEdad(empleados []Empleado, indice int, suma int64) float64 {
	if indice == len(empleados) {
		if len(empleados) == 0 {
			return 0.0
		}
		return float64(suma) / float64(len(empleados))
	}
	return promedioEdad(empleados, indice+1, suma+empleados[indice].edad)
}

// Encontrar por criterio
func empleadoMejorPagado(empleados []Empleado, indice int, mejorIndice int) int {
	if indice == len(empleados) {
		return mejorIndice
	}
	if empleados[indice].salario > empleados[mejorIndice].salario {
		return empleadoMejorPagado(empleados, indice+1, indice)
	}
	return empleadoMejorPagado(empleados, indice+1, mejorIndice)
}

// Clasificación basada en campo
func clasificarPorSalario(salario float64) string {
	if salario < 3000.0 {
		return "junior"
	}
	if salario < 6000.0 {
		return "middle"
	}
	return "senior"
}

// Transformación: crear resumen por departamento
func resumenDepartamento(empleados []Empleado, depto string) Resumen {
	delDepto := filtrarPorDepartamento(empleados, depto, 0, []Empleado{})
	total := contarEmpleados(delDepto, 0, 0)
	salarios := sumarSalarios(delDepto, 0, 0.0)
	return Resumen{
		departamento:   depto,
		totalEmpleados: total,
		salarioTotal:   salarios,
	}
}

// Impresión recursiva
func imprimirEmpleados(empleados []Empleado, indice int) {
	if indice == len(empleados) {
		return
	}
	e := empleados[indice]
	clasif := clasificarPorSalario(e.salario)
	fmt.Printf("  %s (%d años, %s): $%.2f [%s, %d años exp]\n",
		e.nombre, e.edad, e.departamento, e.salario, clasif, e.anosExperiencia)
	imprimirEmpleados(empleados, indice+1)
}

func imprimirResumen(r Resumen) {
	promedio := 0.0
	if r.totalEmpleados > 0 {
		promedio = r.salarioTotal / float64(r.totalEmpleados)
	}
	fmt.Printf("  %s: %d empleados, salario total $%.2f, promedio $%.2f\n",
		r.departamento, r.totalEmpleados, r.salarioTotal, promedio)
}

func main() {
	empleados := []Empleado{
		{"Alice", 28, 4500.00, "Engineering", 5},
		{"Bob", 35, 7200.00, "Engineering", 12},
		{"Carol", 42, 8500.00, "Sales", 18},
		{"Dan", 24, 2800.00, "Engineering", 1},
		{"Eve", 31, 5500.00, "Sales", 7},
		{"Frank", 29, 3200.00, "Sales", 3},
		{"Grace", 45, 9500.00, "Management", 22},
		{"Henry", 33, 6300.00, "Engineering", 9},
	}

	fmt.Println("=== Todos los empleados ===")
	imprimirEmpleados(empleados, 0)

	fmt.Println("\n=== Agregados globales ===")
	total := contarEmpleados(empleados, 0, 0)
	salarioTotal := sumarSalarios(empleados, 0, 0.0)
	edadProm := promedioEdad(empleados, 0, 0)
	fmt.Printf("total empleados: %d\n", total)
	fmt.Printf("salario total: $%.2f\n", salarioTotal)
	fmt.Printf("edad promedio: %.1f\n", edadProm)

	fmt.Println("\n=== Departamento Engineering ===")
	eng := filtrarPorDepartamento(empleados, "Engineering", 0, []Empleado{})
	imprimirEmpleados(eng, 0)

	fmt.Println("\n=== Departamento Sales ===")
	sales := filtrarPorDepartamento(empleados, "Sales", 0, []Empleado{})
	imprimirEmpleados(sales, 0)

	fmt.Println("\n=== Salario >= $5000 ===")
	bienPagados := filtrarPorSalarioMinimo(empleados, 5000.0, 0, []Empleado{})
	imprimirEmpleados(bienPagados, 0)

	fmt.Println("\n=== Senior (10+ años experiencia) ===")
	seniors := filtrarSeniors(empleados, 10, 0, []Empleado{})
	imprimirEmpleados(seniors, 0)

	fmt.Println("\n=== Mejor pagado ===")
	idx := empleadoMejorPagado(empleados, 1, 0)
	mejor := empleados[idx]
	fmt.Printf("  %s con salario $%.2f\n", mejor.nombre, mejor.salario)

	fmt.Println("\n=== Resumen por departamento ===")
	imprimirResumen(resumenDepartamento(empleados, "Engineering"))
	imprimirResumen(resumenDepartamento(empleados, "Sales"))
	imprimirResumen(resumenDepartamento(empleados, "Management"))
}
