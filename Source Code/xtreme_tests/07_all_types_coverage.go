// Test 7: COBERTURA EXHAUSTIVA del sistema de tipos de Go.
// Ejercita cada tipo de dato primitivo, cada formato de literal numérico
// y string, cada tipo compuesto, cada tipo de conversión, cada builtin
// relacionado con tipos (make, new, len, cap, append, copy, delete,
// close, complex, real, imag), valores cero, comparaciones con nil,
// named types vs aliases, y escape sequences completas.
package main

import (
	"errors"
	"fmt"
)

// ================================================================
// 1. LITERALES NUMÉRICOS EN TODAS LAS BASES Y FORMATOS
// ================================================================
const (
	// Enteros decimales
	decimalLit  = 42
	decimalNeg  = -17
	decimalSep  = 1_000_000 // separador _ (Go 1.13+)

	// Hexadecimales (0x y 0X, con separadores)
	hexLow      = 0xff
	hexUp       = 0XFF
	hexSep      = 0xDEAD_BEEF
	hexMixed    = 0xAbC_dEf

	// Octales (nuevo 0o y legacy 0)
	octalNew    = 0o755
	octalUpper  = 0O755
	octalOld    = 0755

	// Binarios
	binaryLow   = 0b1010
	binaryUp    = 0B1010_1010

	// Flotantes decimales
	floatDec    = 3.14
	floatExpLow = 1.5e10
	floatExpUp  = 2.5E-3
	floatNoInt  = .5    // sin parte entera
	floatNoFrac = 1.    // sin parte fraccional

	// Flotante hexadecimal (Go 1.13+)
	floatHex    = 0x1.8p10 // 1.5 * 2^10

	// Imaginarios y complejos
	imagInt     = 2i
	imagFloat   = 1.5i
	imagExp     = 1e3i

	// Runas con cada tipo de escape
	runeSimple  = 'x'
	runeNewline = '\n'
	runeTab     = '\t'
	runeBack    = '\\'
	runeQuote   = '\''
	runeZero    = '\000'
	runeOctal   = '\101'      // 'A'
	runeHex2    = '\x41'      // 'A'
	runeHex4    = '\u00E9'    // 'é'
	runeHex8    = '\U0001F30D' // '🌍'

	// Strings interpretados (con escapes) y crudos (backticks, sin escapes)
	strInterp   = "tab\there\nnewline\\backslash\"quote"
	strRaw      = `raw "string" \no\tescapes
can span multiple lines`

	// Constantes compuestas (expresiones constantes)
	constExpr   = (1 << 10) | 0xF
	constStrCat = "hola" + " " + "mundo"
)

// ================================================================
// 2. TIPOS PRIMITIVOS: cada uno como variable de paquete
// ================================================================
var (
	vBool       bool       = true
	vInt        int        = -1
	vInt8       int8       = -128
	vInt16      int16      = 32_767
	vInt32      int32      = 2_147_483_647
	vInt64      int64      = 9_223_372_036_854_775_807
	vUint       uint       = 1
	vUint8      uint8      = 255
	vUint16     uint16     = 65_535
	vUint32     uint32     = 4_294_967_295
	vUint64     uint64     = 18_446_744_073_709_551_615
	vUintptr    uintptr    = 0xDEADBEEF
	vByte       byte       = 'A' // alias uint8
	vRune       rune       = '語' // alias int32
	vFloat32    float32    = 3.14
	vFloat64    float64    = 3.141592653589793
	vComplex64  complex64  = complex(1, 2)
	vComplex128 complex128 = 3 + 4i
	vString     string     = "hola"
)

// ================================================================
// 3. CONSTANTES TIPADAS, SIN TIPO E IOTA
// ================================================================
const (
	untypedInt    = 1000      // default int al usar
	untypedFloat  = 3.14      // default float64
	untypedStr    = "x"       // default string
	untypedRune   = 'r'       // default rune (int32)
	typedInt   int    = 42
	typedStr   string = "fijo"
)

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

// ================================================================
// 4. NAMED TYPES vs TYPE ALIASES
// ================================================================
type Celsius float64    // named type: identidad nueva
type Fahrenheit float64 // named type independiente
type Temp = Celsius     // alias: MISMA identidad que Celsius
type MyString = string  // alias de builtin

// ================================================================
// 5. STRUCTS (declaraciones usadas más abajo)
// ================================================================
type Point struct{ X, Y int }
type Empty struct{}

type Tagged struct {
	Name string `json:"name" db:"user_name"`
	Age  int    `json:"age,omitempty"`
}

type Embedded struct {
	Tagged     // field embedding (value)
	*Point     // pointer embedding
	Bonus int
}

type Node struct {
	Value      int
	Next, Prev *Node // auto-referencia
}

// Struct con campo de cada tipo posible ("kitchen sink").
type Kitchen struct {
	B     bool
	I     int
	F     float64
	C     complex128
	S     string
	R     rune
	Byt   byte
	Arr   [3]int
	Sl    []int
	Mp    map[string]int
	Sub   Point
	Ptr   *Point
	Fn    func(int) int
	Ch    chan int
	Iface interface{}
	Err   error
	A     any                // alias de interface{}
	Anon  struct{ X, Y int } // struct anónimo inline
	_     int                // campo blank (sin nombre, para padding)
}

// ================================================================
// 6. ARRAYS (tamaño fijo, semántica por valor)
// ================================================================
var (
	arrFixed    = [5]int{1, 2, 3, 4, 5}
	arrInferred = [...]int{10, 20, 30}             // longitud inferida
	arrByIndex  = [5]int{0: 1, 4: 5}               // índices explícitos
	arr2D       = [2][3]int{{1, 2, 3}, {4, 5, 6}}
	arrEmpty    = [0]int{}
	arrStrings  = [3]string{"a", "b"}              // [2] queda zero-value
	arrStructs  = [2]Point{{1, 2}, {3, 4}}
)

// ================================================================
// 7. SLICES
// ================================================================
var (
	sliceLit     = []int{1, 2, 3}
	sliceMake    = make([]int, 5)         // len=5 cap=5
	sliceMakeCap = make([]int, 3, 10)     // len=3 cap=10
	sliceNil     []int                    // == nil, len=0, cap=0
	sliceBytes   = []byte("abc")
	sliceRunes   = []rune("héllo🌍")
	slice2D      = [][]int{{1, 2}, {3, 4, 5}, nil}
	slicePtrs    = []*int{new(int), nil}
	sliceStructs = []Point{{1, 2}, {3, 4}}
	sliceOfMaps  = []map[string]int{{"a": 1}, {"b": 2}}
	sliceOfFns   = []func(int) int{
		func(x int) int { return x + 1 },
		func(x int) int { return x * 2 },
	}
)

// ================================================================
// 8. MAPS
// ================================================================
var (
	mapLit         = map[string]int{"a": 1, "b": 2}
	mapMake        = make(map[int]string)
	mapMakeCap     = make(map[string][]int, 100)
	mapNil         map[string]int // == nil
	mapStructKey   = map[Point]string{{0, 0}: "origin", {1, 1}: "diag"}
	mapNested      = map[string]map[string]int{"g1": {"a": 1}, "g2": {"c": 3}}
	mapEmptyStruct = map[string]struct{}{"a": {}, "b": {}} // set pattern
	mapOfFuncs     = map[string]func(int) int{
		"inc": func(x int) int { return x + 1 },
	}
)

// ================================================================
// 9. PUNTEROS (incluyendo puntero-a-puntero y punteros a compuestos)
// ================================================================
var (
	pInt     = new(int)       // *int apuntando a zero
	ppInt    = &pInt          // **int
	pArr     = &[3]int{1, 2, 3}
	pStruct  = &Point{1, 2}
	pSlice   = &[]int{1, 2, 3} // raro pero legal
	pFnNil   *func()           // nil
)

// ================================================================
// 10. TIPOS FUNCIÓN
// ================================================================
type UnaryOp   func(int) int
type BinaryOp  func(int, int) int
type Variadic  func(...int) int
type MultiRet  func(int) (int, error)
type NamedRet  func(a, b int) (sum, diff int)
type HigherOrd func(UnaryOp) UnaryOp
type VoidVoid  func()

// ================================================================
// 11. INTERFACES (vacía, con métodos, embebida)
// ================================================================
type Any0     interface{}   // equivalente a any
type Any1     = interface{} // alias
type Stringer interface{ String() string }
type Reader   interface{ Read(p []byte) (int, error) }
type Writer   interface{ Write(p []byte) (int, error) }
type RW       interface {
	Reader
	Writer
}

// ================================================================
// 12. CHANNELS (todas las variantes)
// ================================================================
var (
	chUnbuf   = make(chan int)         // unbuffered
	chBuf     = make(chan int, 10)     // buffered
	chSendO   chan<- int               // direccional envío (nil aquí)
	chRecvO   <-chan int               // direccional recepción
	chOfCh    = make(chan chan int)    // canal de canales
	chOfFn    = make(chan func())      // canal de funciones
	chOfEmpty = make(chan struct{})    // señalización sin payload
)

// ================================================================
// 13. ERROR TYPE
// ================================================================
type MyError struct {
	Code int
	Msg  string
}

func (e *MyError) Error() string { return fmt.Sprintf("E%d: %s", e.Code, e.Msg) }

var ErrSentinel = errors.New("sentinel")

// Métodos con receptor valor y receptor puntero sobre el mismo tipo.
type Counter struct{ n int }

func (c Counter) Value() int { return c.n }    // receptor por valor
func (c *Counter) Inc()      { c.n++ }          // receptor por puntero

// ================================================================
// USO EN main(): fuerza que todo se compile y comprueba comportamiento.
// ================================================================
func main() {
	// --- Literales (evitar "declared and not used" a nivel de print) ---
	fmt.Println("nums:", decimalLit, decimalNeg, decimalSep,
		hexLow, hexUp, hexSep, hexMixed,
		octalNew, octalUpper, octalOld,
		binaryLow, binaryUp,
		floatDec, floatExpLow, floatExpUp, floatNoInt, floatNoFrac, floatHex,
		imagInt, imagFloat, imagExp,
		constExpr, constStrCat)

	fmt.Println("runes:", runeSimple, runeNewline, runeTab, runeBack, runeQuote,
		runeZero, runeOctal, runeHex2, runeHex4, runeHex8)
	fmt.Println("strings:", strInterp)
	fmt.Println(strRaw)

	fmt.Println("constants:", untypedInt, untypedFloat, untypedStr, untypedRune,
		typedInt, typedStr)

	fmt.Println("primitives:", vBool, vInt, vInt8, vInt16, vInt32, vInt64,
		vUint, vUint8, vUint16, vUint32, vUint64, vUintptr,
		vByte, vRune, vFloat32, vFloat64, vComplex64, vComplex128, vString)

	// --- Builtins de números complejos ---
	z := complex(3.0, 4.0)
	fmt.Println("re=", real(z), "im=", imag(z))

	// --- Conversiones entre numéricos ---
	var xi int = 10
	var xf float64 = float64(xi)
	var xi32 int32 = int32(xf)
	fmt.Println(xi, xf, xi32)

	// string <-> []byte <-> []rune (conversiones de builtin)
	s := "héllo"
	bs := []byte(s)
	rs := []rune(s)
	fmt.Println(bs, rs, string(bs), string(rs))

	// byte <-> rune <-> int
	var by byte = 'A'
	var rn rune = rune(by)
	var in int = int(rn)
	fmt.Println(by, rn, in, string(rn))

	// Named type conversions
	c := Celsius(100)
	f := Fahrenheit(float64(c)*9/5 + 32)
	var t Temp = c // alias: no requiere conversión
	fmt.Println(c, f, t)

	// --- iota enum ---
	today := Wednesday
	fmt.Println("today =", today)

	// --- Arrays / slices ---
	fmt.Println(arrFixed, arrInferred, arrByIndex, arr2D, arrEmpty, arrStrings, arrStructs)
	fmt.Println(sliceLit, sliceMake, sliceMakeCap, sliceNil == nil,
		sliceBytes, sliceRunes, slice2D, sliceStructs, sliceOfMaps)
	fmt.Println(slicePtrs[0] != nil, slicePtrs[1] == nil)
	fmt.Println(sliceOfFns[0](5), sliceOfFns[1](5))

	// builtins: append, copy, len, cap
	a := []int{1, 2, 3}
	b := append(a, 4, 5)
	cpy := make([]int, 3)
	ncopied := copy(cpy, a)
	fmt.Println(a, b, cpy, ncopied, len(b), cap(b))

	// three-index slicing
	sub := b[1:3:4]
	fmt.Println("sub:", sub, "len=", len(sub), "cap=", cap(sub))

	// --- Maps ---
	fmt.Println(mapLit, mapMake, mapMakeCap, mapNil == nil,
		mapStructKey, mapNested, mapEmptyStruct)
	delete(mapLit, "a") // builtin delete
	v, ok := mapLit["b"]
	fmt.Println(v, ok)
	_, missing := mapLit["xxx"]
	fmt.Println(missing)
	fmt.Println(mapOfFuncs["inc"](41))

	// --- Structs ---
	e := Embedded{
		Tagged: Tagged{Name: "Bob", Age: 30},
		Point:  &Point{9, 9},
		Bonus:  100,
	}
	fmt.Println(e.Name, e.Age, e.Bonus, e.X, e.Y) // todos promoted

	// igualdad de structs (son comparables si todos sus campos lo son)
	p1 := Point{1, 2}
	p2 := Point{1, 2}
	fmt.Println(p1 == p2)

	// lista doblemente enlazada recursiva
	head := &Node{Value: 1}
	mid := &Node{Value: 2, Prev: head}
	tail := &Node{Value: 3, Prev: mid}
	head.Next = mid
	mid.Next = tail
	fmt.Println(head.Value, head.Next.Value, head.Next.Next.Value, tail.Prev.Prev.Value)

	// kitchen sink (un campo de cada tipo)
	k := Kitchen{
		B: true, I: 1, F: 2.0, C: 1 + 1i, S: "s", R: 'ñ', Byt: 0xFF,
		Arr: [3]int{1, 2, 3}, Sl: []int{1}, Mp: map[string]int{"k": 1},
		Sub: Point{0, 0}, Ptr: nil, Fn: func(x int) int { return x },
		Ch: make(chan int, 1), Iface: "cualquiera", Err: nil,
		A: 42, Anon: struct{ X, Y int }{1, 2},
	}
	fmt.Println(k.B, k.I, k.Anon.X)

	// --- Punteros ---
	*pInt = 50
	fmt.Println(*pInt, **ppInt, (*pArr)[1], pStruct.X, (*pSlice)[0], pFnNil == nil)

	// new vs &Type{}
	pNew := new(Point)
	pLit := &Point{5, 6}
	fmt.Println(*pNew, *pLit)

	// --- Funciones como tipo de primera clase ---
	var op UnaryOp = func(x int) int { return x * x }
	var add BinaryOp = func(a, b int) int { return a + b }
	var sumN Variadic = func(nums ...int) int {
		total := 0
		for _, n := range nums {
			total += n
		}
		return total
	}
	var mr MultiRet = func(x int) (int, error) {
		if x < 0 {
			return 0, &MyError{Code: 1, Msg: "negativo"}
		}
		return x, nil
	}
	var nr NamedRet = func(a, b int) (sum, diff int) {
		sum = a + b
		diff = a - b
		return // naked return
	}
	var ho HigherOrd = func(f UnaryOp) UnaryOp {
		return func(x int) int { return f(f(x)) }
	}
	var voidFn VoidVoid = func() { fmt.Println("void!") }

	fmt.Println(op(3), add(2, 3), sumN(1, 2, 3, 4))
	if r, err := mr(-1); err != nil {
		fmt.Println("error:", err)
	} else {
		fmt.Println(r)
	}
	s1, d1 := nr(10, 3)
	fmt.Println(s1, d1)
	doubleSquare := ho(op)
	fmt.Println(doubleSquare(2)) // ((2^2)^2) = 16
	voidFn()

	// variadic spread: pasar slice con ...
	nums := []int{10, 20, 30}
	fmt.Println(sumN(nums...))

	// --- Interfaces ---
	var iface Any0 = 42
	switch v := iface.(type) {
	case int:
		fmt.Println("int:", v)
	case string:
		fmt.Println("string:", v)
	default:
		fmt.Println("otro")
	}
	var _ Any1 = "alias anónimo de interface{}"

	var anyV any = "desde any alias"
	fmt.Println(anyV)

	// error type
	var err error = &MyError{Code: 404, Msg: "not found"}
	fmt.Println(err)
	err2 := fmt.Errorf("wrapped: %w", ErrSentinel)
	fmt.Println(err2, errors.Is(err2, ErrSentinel))

	// --- Methods con receptor valor vs puntero ---
	ctr := Counter{n: 0}
	(&ctr).Inc() // requiere puntero
	ctr.Inc()    // Go auto-toma dirección
	fmt.Println(ctr.Value())

	// --- Channels: todos los usos básicos ---
	chBuf <- 1
	chBuf <- 2
	close(chBuf) // builtin close
	for v := range chBuf {
		fmt.Print(v, " ")
	}
	fmt.Println()

	// evitar "declared and not used" - referenciamos los canales restantes
	_ = chUnbuf
	_ = chSendO
	_ = chRecvO
	_ = chOfCh
	_ = chOfFn
	_ = chOfEmpty

	// --- Comparaciones con nil para cada tipo nilable ---
	var (
		n1 *int
		n2 []int
		n3 map[string]int
		n4 chan int
		n5 func()
		n6 interface{}
	)
	fmt.Println(n1 == nil, n2 == nil, n3 == nil, n4 == nil, n5 == nil, n6 == nil)

	// --- Valores cero (zero values) ---
	var (
		zb  bool
		zi  int
		zf  float64
		zs  string
		zc  complex128
		zar [3]int
		zp  Point
		ze  Empty
	)
	fmt.Printf("zeros: %v %v %v %q %v %v %v %v\n", zb, zi, zf, zs, zc, zar, zp, ze)

	// --- Asignación múltiple y swap ---
	aa, bb := 1, 2
	aa, bb = bb, aa
	fmt.Println(aa, bb)

	// blank identifier
	_, second := "x", "y"
	fmt.Println(second)

	// --- Len de string cuenta bytes; rune-count requiere conversión ---
	fmt.Println(len("héllo"), len([]rune("héllo")))

	// --- Comparación de arrays (comparables) ---
	a1 := [3]int{1, 2, 3}
	a2 := [3]int{1, 2, 3}
	fmt.Println(a1 == a2)

	// --- Uso de panic/recover como builtins ---
	defer func() {
		if r := recover(); r != nil {
			fmt.Println("recovered:", r)
		}
	}()
	doPanic := func() { panic("prueba de builtin panic") }
	doPanic()
}
