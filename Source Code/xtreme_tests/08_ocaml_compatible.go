// Test 8: SUBCONJUNTO DE GO QUE MAPEA NATURALMENTE A OCAML.
// Cada sección incluye el equivalente OCaml en un comentario.
//
// INCLUIDO (mapeo limpio a OCaml):
//   - Tipos primitivos, named types, enums vía iota
//   - Structs (records), métodos, embedding por composición
//   - Sum types vía marker interface + type switch (→ variants)
//   - Recursión, recursión mutua, tipos recursivos
//   - Slices/arrays (→ array o list), maps (→ Hashtbl/Map)
//   - Closures, higher-order functions, currying
//   - Multi-return values (→ tuplas), error (→ result)
//   - Punteros para mutabilidad (→ ref), option pattern con (T, bool)
//   - Genéricos simples sin constraints exóticas (→ polimorfismo paramétrico)
//   - Control de flujo: if/else, for, switch (→ match)
//
// EXCLUIDO (no mapea o requiere infraestructura no trivial):
//   - goroutines, channels, select, go statements
//   - nil polimórfico, reflection, struct tags
//   - defer, panic/recover, goto, labeled break
//   - unsafe, complex numbers, iota con bit-shifting complejo
//   - constraints con tilde (~), interfaces como tipos existenciales
package main

import (
	"errors"
	"fmt"
	"sort"
	"strings"
)

// ================================================================
// 1. TIPOS PRIMITIVOS Y NAMED TYPES
// OCaml: bool, int, float, string, char
// Named types → `type celsius = Celsius of float` o abstract types
// ================================================================
type Celsius float64
type Fahrenheit float64
type UserID int
type Username string

func celsiusToFahrenheit(c Celsius) Fahrenheit {
	return Fahrenheit(float64(c)*9.0/5.0 + 32.0)
}

// ================================================================
// 2. ENUMS VÍA IOTA (forma simple, sin bit-flags)
// OCaml: `type weekday = Sunday | Monday | ... | Saturday`
// ================================================================
type Weekday int

const (
	Sunday Weekday = iota
	Monday
	Tuesday
	Wednesday
	Thursday
	Friday
	Saturday
)

func (w Weekday) String() string {
	names := [...]string{"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"}
	return names[w]
}

func isWeekend(w Weekday) bool {
	return w == Sunday || w == Saturday
}

type Status int

const (
	StatusPending Status = iota
	StatusActive
	StatusClosed
	StatusArchived
)

// ================================================================
// 3. STRUCTS → RECORDS
// OCaml: `type point = { x : float; y : float }`
// ================================================================
type Point struct {
	X, Y float64
}

type Rectangle struct {
	TopLeft, BottomRight Point
}

type Person struct {
	Name string
	Age  int
	ID   UserID
}

// ================================================================
// 4. MÉTODOS CON RECEPTOR POR VALOR Y POR PUNTERO
// OCaml: funciones regulares que toman el record;
//   receptor-puntero → record con mutable fields + función que muta
// ================================================================

// Receptor por valor: no muta, retorna nuevo.
// OCaml: let translate p dx dy = { x = p.x +. dx; y = p.y +. dy }
func (p Point) Translate(dx, dy float64) Point {
	return Point{X: p.X + dx, Y: p.Y + dy}
}

// Receptor por valor con cómputo.
func (p Point) DistanceSquared(other Point) float64 {
	dx := p.X - other.X
	dy := p.Y - other.Y
	return dx*dx + dy*dy
}

func (r Rectangle) Width() float64  { return r.BottomRight.X - r.TopLeft.X }
func (r Rectangle) Height() float64 { return r.BottomRight.Y - r.TopLeft.Y }
func (r Rectangle) Area() float64   { return r.Width() * r.Height() }

// ================================================================
// 5. STRUCT EMBEDDING → COMPOSICIÓN DE RECORDS
// OCaml: record con campo del tipo embebido; acceso explícito.
// (No hay method promotion automático, pero es directo traducir.)
// ================================================================
type Animal struct {
	Name    string
	Species string
}

func (a Animal) Describe() string {
	return fmt.Sprintf("%s (%s)", a.Name, a.Species)
}

type Dog struct {
	Animal      // en OCaml: campo `animal : animal`
	Breed  string
}

func (d Dog) FullDescription() string {
	// En OCaml accederíamos como d.animal.name explícitamente.
	return fmt.Sprintf("%s, raza %s", d.Describe(), d.Breed)
}

// ================================================================
// 6. SUM TYPES VÍA MARKER INTERFACE → VARIANTS
// Este es el patrón que MEJOR mapea a OCaml. El type switch se
// convierte directamente en pattern matching.
//
// OCaml:
//   type expr =
//     | Num of float
//     | Var of string
//     | Add of expr * expr
//     | Mul of expr * expr
//     | Neg of expr
//     | Let of string * expr * expr
// ================================================================
type Expr interface {
	isExpr()
}

type Num struct{ Value float64 }
type Var struct{ Name string }
type Add struct{ Left, Right Expr }
type Mul struct{ Left, Right Expr }
type Neg struct{ E Expr }
type Let struct {
	Name string
	Val  Expr
	Body Expr
}

func (Num) isExpr() {}
func (Var) isExpr() {}
func (Add) isExpr() {}
func (Mul) isExpr() {}
func (Neg) isExpr() {}
func (Let) isExpr() {}

// Evaluador: type switch → match en OCaml.
func eval(e Expr, env map[string]float64) float64 {
	switch v := e.(type) {
	case Num:
		return v.Value
	case Var:
		return env[v.Name]
	case Add:
		return eval(v.Left, env) + eval(v.Right, env)
	case Mul:
		return eval(v.Left, env) * eval(v.Right, env)
	case Neg:
		return -eval(v.E, env)
	case Let:
		newEnv := make(map[string]float64, len(env)+1)
		for k, val := range env {
			newEnv[k] = val
		}
		newEnv[v.Name] = eval(v.Val, env)
		return eval(v.Body, newEnv)
	}
	return 0
}

// Otro sum type: árbol binario.
// OCaml: type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree
type Tree interface {
	isTree()
}

type Leaf struct{}
type TNode struct {
	Left  Tree
	Value int
	Right Tree
}

func (Leaf) isTree()  {}
func (TNode) isTree() {}

func insert(t Tree, v int) Tree {
	switch n := t.(type) {
	case Leaf:
		return TNode{Left: Leaf{}, Value: v, Right: Leaf{}}
	case TNode:
		if v < n.Value {
			return TNode{Left: insert(n.Left, v), Value: n.Value, Right: n.Right}
		} else if v > n.Value {
			return TNode{Left: n.Left, Value: n.Value, Right: insert(n.Right, v)}
		}
		return n
	}
	return t
}

func inOrder(t Tree) []int {
	switch n := t.(type) {
	case Leaf:
		return nil
	case TNode:
		var result []int
		result = append(result, inOrder(n.Left)...)
		result = append(result, n.Value)
		result = append(result, inOrder(n.Right)...)
		return result
	}
	return nil
}

// ================================================================
// 7. LISTAS RECURSIVAS (patrón clásico)
// OCaml: type 'a list = Nil | Cons of 'a * 'a list
// (OCaml tiene listas builtin, pero así se ve el patrón.)
// ================================================================
type IntList interface {
	isIntList()
}

type Nil struct{}
type Cons struct {
	Head int
	Tail IntList
}

func (Nil) isIntList()  {}
func (Cons) isIntList() {}

func listLen(l IntList) int {
	switch v := l.(type) {
	case Nil:
		return 0
	case Cons:
		return 1 + listLen(v.Tail)
	}
	return 0
}

func listSum(l IntList) int {
	switch v := l.(type) {
	case Nil:
		return 0
	case Cons:
		return v.Head + listSum(v.Tail)
	}
	return 0
}

// ================================================================
// 8. ARRAYS, SLICES Y MAPS
// OCaml: array → 'a array (mutable), slice → 'a array o 'a list,
//        map → (K, V) Hashtbl.t o Map.Make(K).t
// ================================================================

func sumSlice(xs []int) int {
	total := 0
	for _, x := range xs {
		total += x
	}
	return total
}

func reverseSlice(xs []int) []int {
	n := len(xs)
	result := make([]int, n)
	for i, x := range xs {
		result[n-1-i] = x
	}
	return result
}

// Búsqueda linear: retorna (índice, encontrado).
// OCaml: int option (None | Some idx) o la tupla (int, bool).
func findIndex(xs []int, target int) (int, bool) {
	for i, x := range xs {
		if x == target {
			return i, true
		}
	}
	return -1, false
}

// Count words: map[string]int
// OCaml: let tbl = Hashtbl.create 16 in ...
func countWords(words []string) map[string]int {
	counts := make(map[string]int)
	for _, w := range words {
		counts[w]++
	}
	return counts
}

// Lookup con el idiom (value, ok).
// OCaml: Hashtbl.find_opt: 'a option
func lookupAge(m map[string]int, name string) (int, bool) {
	age, ok := m[name]
	return age, ok
}

// ================================================================
// 9. HIGHER-ORDER FUNCTIONS (el puente natural con OCaml)
// OCaml: let rec map f = function [] -> [] | x :: xs -> f x :: map f xs
// ================================================================

func mapInt(xs []int, f func(int) int) []int {
	result := make([]int, len(xs))
	for i, x := range xs {
		result[i] = f(x)
	}
	return result
}

func filterInt(xs []int, pred func(int) bool) []int {
	var result []int
	for _, x := range xs {
		if pred(x) {
			result = append(result, x)
		}
	}
	return result
}

func foldLeftInt(xs []int, init int, f func(int, int) int) int {
	acc := init
	for _, x := range xs {
		acc = f(acc, x)
	}
	return acc
}

// ================================================================
// 10. GENÉRICOS SIMPLES → POLIMORFISMO PARAMÉTRICO
// Sin constraints exóticas — sólo `any` y `comparable`.
// OCaml: 'a, 'b (polimorfismo natural sin anotación).
// ================================================================

func Map[T, U any](xs []T, f func(T) U) []U {
	result := make([]U, len(xs))
	for i, x := range xs {
		result[i] = f(x)
	}
	return result
}

func Filter[T any](xs []T, pred func(T) bool) []T {
	var result []T
	for _, x := range xs {
		if pred(x) {
			result = append(result, x)
		}
	}
	return result
}

func Fold[T, U any](xs []T, init U, f func(U, T) U) U {
	acc := init
	for _, x := range xs {
		acc = f(acc, x)
	}
	return acc
}

// Pair genérico → OCaml: type ('a, 'b) pair = { first : 'a; second : 'b }
type Pair[A, B any] struct {
	First  A
	Second B
}

func MakePair[A, B any](a A, b B) Pair[A, B] {
	return Pair[A, B]{First: a, Second: b}
}

// ================================================================
// 11. CLOSURES Y CURRYING
// Natural en OCaml: let make_adder n = fun x -> x + n
// ================================================================

func makeAdder(n int) func(int) int {
	return func(x int) int { return x + n }
}

// Closure con estado mutable (→ OCaml: ref capturado en closure).
func makeCounter() func() int {
	count := 0
	return func() int {
		count++
		return count
	}
}

// Currying explícito.
func curry(f func(int, int) int) func(int) func(int) int {
	return func(a int) func(int) int {
		return func(b int) int { return f(a, b) }
	}
}

// Composición de funciones.
func compose[A, B, C any](f func(B) C, g func(A) B) func(A) C {
	return func(x A) C { return f(g(x)) }
}

// ================================================================
// 12. RECURSIÓN (directa y mutua)
// OCaml: let rec f ... = ... and g ... = ...
// ================================================================

func factorial(n int) int {
	if n <= 1 {
		return 1
	}
	return n * factorial(n-1)
}

func fibMemo() func(int) int {
	cache := map[int]int{0: 0, 1: 1}
	var fib func(int) int
	fib = func(n int) int {
		if v, ok := cache[n]; ok {
			return v
		}
		r := fib(n-1) + fib(n-2)
		cache[n] = r
		return r
	}
	return fib
}

// Recursión mutua: OCaml usa `let rec ... and ...`
func isEven(n int) bool {
	if n == 0 {
		return true
	}
	return isOdd(n - 1)
}

func isOdd(n int) bool {
	if n == 0 {
		return false
	}
	return isEven(n - 1)
}

// ================================================================
// 13. MULTI-RETURN Y MANEJO DE ERRORES
// OCaml: tuplas directamente, o result para errores.
//   type ('a, 'e) result = Ok of 'a | Error of 'e
// ================================================================

func divide(a, b float64) (float64, error) {
	if b == 0 {
		return 0, errors.New("división por cero")
	}
	return a / b, nil
}

// Parse emulado con (valor, error) → OCaml result.
func parsePositive(s string) (int, error) {
	var n int
	_, err := fmt.Sscanf(s, "%d", &n)
	if err != nil {
		return 0, fmt.Errorf("no es número: %q", s)
	}
	if n < 0 {
		return 0, fmt.Errorf("negativo: %d", n)
	}
	return n, nil
}

// Min y max como tupla.
func minMax(xs []int) (int, int) {
	if len(xs) == 0 {
		return 0, 0
	}
	mn, mx := xs[0], xs[0]
	for _, x := range xs[1:] {
		if x < mn {
			mn = x
		}
		if x > mx {
			mx = x
		}
	}
	return mn, mx
}

// ================================================================
// 14. PUNTEROS COMO REFS MUTABLES
// OCaml: 'a ref — `let r = ref 0 in !r, r := 5`
// Sólo para mutabilidad local; evitamos nil semántico aquí.
// ================================================================

func incrementRef(n *int) {
	*n++
}

func swapInts(a, b *int) {
	*a, *b = *b, *a
}

// ================================================================
// 15. STRUCT CON ESTADO MUTABLE (stack simple)
// OCaml: record con campo mutable, o módulo con estado.
// ================================================================
type Stack struct {
	items []int
}

func NewStack() *Stack { return &Stack{} }

func (s *Stack) Push(v int) { s.items = append(s.items, v) }

// Pop devuelve (value, ok) → OCaml: int option
func (s *Stack) Pop() (int, bool) {
	if len(s.items) == 0 {
		return 0, false
	}
	last := len(s.items) - 1
	v := s.items[last]
	s.items = s.items[:last]
	return v, true
}

func (s *Stack) Len() int { return len(s.items) }

// ================================================================
// 16. CONTROL DE FLUJO
// if/else, for (3 formas: counter, while, range), switch — todos
// tienen equivalente directo en OCaml.
// ================================================================

// switch sin fallthrough equivale a match.
func classify(n int) string {
	switch {
	case n < 0:
		return "negativo"
	case n == 0:
		return "cero"
	case n < 10:
		return "pequeño"
	case n < 100:
		return "mediano"
	default:
		return "grande"
	}
}

// switch con valor concreto → match en OCaml.
func dayKind(w Weekday) string {
	switch w {
	case Saturday, Sunday:
		return "finde"
	case Friday:
		return "casi finde"
	default:
		return "laboral"
	}
}

// for como while
func gcd(a, b int) int {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

// for con counter
func power(base, exp int) int {
	result := 1
	for i := 0; i < exp; i++ {
		result *= base
	}
	return result
}

// ================================================================
// 17. OPTION PATTERN EMULADO
// Go usa (T, bool) o (T, error). OCaml lo expresa como 'a option.
// Aquí mostramos ambos idioms lado a lado.
// ================================================================

func safeDivide(a, b int) (int, bool) {
	if b == 0 {
		return 0, false // en OCaml: None
	}
	return a / b, true // en OCaml: Some (a / b)
}

func firstEven(xs []int) (int, bool) {
	for _, x := range xs {
		if x%2 == 0 {
			return x, true
		}
	}
	return 0, false
}

// ================================================================
// 18. STRINGS (operaciones básicas)
// OCaml: módulo String, String.length, String.concat, etc.
// ================================================================

func normalizeSpaces(s string) string {
	return strings.Join(strings.Fields(s), " ")
}

func isPalindrome(s string) bool {
	n := len(s)
	for i := 0; i < n/2; i++ {
		if s[i] != s[n-1-i] {
			return false
		}
	}
	return true
}

// ================================================================
// MAIN: ejercita todas las secciones.
// ================================================================
func main() {
	// 1. Named types
	c := Celsius(100)
	fmt.Printf("%.1f°C = %.1f°F\n", float64(c), float64(celsiusToFahrenheit(c)))

	// 2. Enums
	today := Wednesday
	fmt.Printf("hoy: %s (finde? %v)\n", today, isWeekend(today))

	// 3-4. Structs y métodos
	p1 := Point{X: 1, Y: 2}
	p2 := p1.Translate(3, 4)
	fmt.Println("translated:", p2, "dist²:", p1.DistanceSquared(p2))

	r := Rectangle{TopLeft: Point{0, 0}, BottomRight: Point{5, 3}}
	fmt.Printf("rect area = %.1f\n", r.Area())

	// 5. Embedding
	rex := Dog{Animal: Animal{Name: "Rex", Species: "Canis"}, Breed: "Labrador"}
	fmt.Println(rex.FullDescription())

	// 6. Sum type: evaluador
	//    let x = 3 in (x + 4) * (-x)  →  3+4 = 7, *-3 = -21
	expr := Let{
		Name: "x",
		Val:  Num{3},
		Body: Mul{
			Left:  Add{Left: Var{"x"}, Right: Num{4}},
			Right: Neg{E: Var{"x"}},
		},
	}
	fmt.Println("eval:", eval(expr, nil))

	// Árbol binario
	var t Tree = Leaf{}
	for _, v := range []int{5, 3, 8, 1, 4, 7, 9} {
		t = insert(t, v)
	}
	fmt.Println("inorder:", inOrder(t))

	// 7. Lista recursiva
	list := Cons{Head: 1, Tail: Cons{Head: 2, Tail: Cons{Head: 3, Tail: Nil{}}}}
	fmt.Println("len:", listLen(list), "sum:", listSum(list))

	// 8. Slices y maps
	nums := []int{3, 1, 4, 1, 5, 9, 2, 6}
	fmt.Println("sum:", sumSlice(nums), "reversed:", reverseSlice(nums))
	if idx, ok := findIndex(nums, 5); ok {
		fmt.Println("5 encontrado en índice", idx)
	}

	wc := countWords([]string{"foo", "bar", "foo", "baz", "bar", "foo"})
	// Orden determinista para output estable.
	keys := make([]string, 0, len(wc))
	for k := range wc {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	for _, k := range keys {
		fmt.Printf("  %s: %d\n", k, wc[k])
	}

	ages := map[string]int{"Alice": 30, "Bob": 25}
	if age, ok := lookupAge(ages, "Alice"); ok {
		fmt.Println("Alice:", age)
	}

	// 9-10. Higher-order y genéricos
	doubled := mapInt(nums, func(x int) int { return x * 2 })
	evens := filterInt(nums, func(x int) bool { return x%2 == 0 })
	total := foldLeftInt(nums, 0, func(a, b int) int { return a + b })
	fmt.Println("doubled:", doubled, "evens:", evens, "total:", total)

	words := []string{"hola", "mundo", "ocaml"}
	lens := Map(words, func(s string) int { return len(s) })
	fmt.Println("longitudes:", lens)

	pair := MakePair("edad", 30)
	fmt.Printf("%+v\n", pair)

	// 11. Closures y currying
	add3 := makeAdder(3)
	fmt.Println("add3(10) =", add3(10))

	tick := makeCounter()
	fmt.Println(tick(), tick(), tick())

	add := curry(func(a, b int) int { return a + b })
	fmt.Println("curry:", add(5)(7))

	inc := func(x int) int { return x + 1 }
	dbl := func(x int) int { return x * 2 }
	incThenDbl := compose(dbl, inc)
	fmt.Println("compose:", incThenDbl(4)) // (4+1)*2 = 10

	// 12. Recursión
	fmt.Println("5! =", factorial(5))
	fib := fibMemo()
	fibs := make([]int, 10)
	for i := range fibs {
		fibs[i] = fib(i)
	}
	fmt.Println("fib:", fibs)
	fmt.Println("even(10)?", isEven(10), "odd(7)?", isOdd(7))

	// 13. Multi-return y error
	if q, err := divide(22, 7); err == nil {
		fmt.Printf("22/7 ≈ %.4f\n", q)
	}
	if _, err := divide(1, 0); err != nil {
		fmt.Println("err esperado:", err)
	}
	if n, err := parsePositive("42"); err == nil {
		fmt.Println("parsed:", n)
	}
	mn, mx := minMax(nums)
	fmt.Println("min:", mn, "max:", mx)

	// 14. Ref mutation
	x := 10
	incrementRef(&x)
	fmt.Println("x =", x)
	a, b := 1, 2
	swapInts(&a, &b)
	fmt.Println("swap:", a, b)

	// 15. Stack
	st := NewStack()
	for _, v := range []int{1, 2, 3, 4} {
		st.Push(v)
	}
	fmt.Print("stack pop: ")
	for st.Len() > 0 {
		if v, ok := st.Pop(); ok {
			fmt.Print(v, " ")
		}
	}
	fmt.Println()

	// 16. Control de flujo
	for _, n := range []int{-5, 0, 7, 50, 500} {
		fmt.Printf("%d → %s; ", n, classify(n))
	}
	fmt.Println()
	fmt.Println(dayKind(Friday), dayKind(Sunday), dayKind(Tuesday))
	fmt.Println("gcd(48,18) =", gcd(48, 18))
	fmt.Println("2^10 =", power(2, 10))

	// 17. Option pattern
	if q, ok := safeDivide(10, 3); ok {
		fmt.Println("10/3 =", q)
	}
	if v, ok := firstEven([]int{1, 3, 5, 8, 9}); ok {
		fmt.Println("primer par:", v)
	}

	// 18. Strings
	fmt.Printf("[%s]\n", normalizeSpaces("   hola    mundo  "))
	for _, w := range []string{"anilina", "hola", "reconocer", "abba"} {
		fmt.Printf("%s palíndromo? %v\n", w, isPalindrome(w))
	}
}
