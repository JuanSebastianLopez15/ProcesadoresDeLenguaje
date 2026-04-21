type celsius = float

type fahrenheit = float

type userid = int

type username = string

let rec celsiustofahrenheit (c : float) : float =
let module __ret = struct exception E of float end in
try
raise (__ret.E ((((c *. 9.) /. 5.) +. 32.)));
0.0
with
| __ret.E v -> v

type weekday = int

let sunday : int = 0

let monday : int = 1

let tuesday : int = 2

let wednesday : int = 3

let thursday : int = 4

let friday : int = 5

let saturday : int = 6

let rec string (w : int) : string =
let module __ret = struct exception E of string end in
try
let names = ref [|"Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"|] in (
raise (__ret.E ((!names).(w))));
""
with
| __ret.E v -> v

let rec isweekend (w : int) : bool =
let module __ret = struct exception E of bool end in
try
raise (__ret.E (((w = sunday) || (w = saturday))));
false
with
| __ret.E v -> v

type status = int

let statuspending : int = 0

let statusactive : int = 1

let statusclosed : int = 2

let statusarchived : int = 3

type point = { mutable x : float; mutable y : float; }

type rectangle = { mutable topleft : point; mutable bottomright : point; }

type person = { mutable name : string; mutable age : int; mutable id : userid; }

let rec translate (p : point) (dx : float) (dy : float) : point =
let module __ret = struct exception E of point end in
try
raise (__ret.E ({ x = (p.x +. dx); y = (p.y +. dy) }));
Obj.magic ()
with
| __ret.E v -> v

let rec distancesquared (p : point) (other : point) : float =
let module __ret = struct exception E of float end in
try
let dx = ref (p.x -. other.x) in (
let dy = ref (p.y -. other.y) in (
raise (__ret.E (((!dx * !dx) + (!dy * !dy))))));
0.0
with
| __ret.E v -> v

let rec width (r : rectangle) : float =
let module __ret = struct exception E of float end in
try
raise (__ret.E ((r.bottomright.x -. r.topleft.x)));
0.0
with
| __ret.E v -> v

let rec height (r : rectangle) : float =
let module __ret = struct exception E of float end in
try
raise (__ret.E ((r.bottomright.y -. r.topleft.y)));
0.0
with
| __ret.E v -> v

let rec area (r : rectangle) : float =
let module __ret = struct exception E of float end in
try
raise (__ret.E (((width r) * (height r))));
0.0
with
| __ret.E v -> v

type animal = { mutable name : string; mutable species : string; }

let rec describe (a : animal) : string =
let module __ret = struct exception E of string end in
try
raise (__ret.E (Printf.sprintf "%s (%s)" a.name a.species));
""
with
| __ret.E v -> v

type dog = { mutable _embedded : animal; mutable breed : string; }

let rec fulldescription (d : dog) : string =
let module __ret = struct exception E of string end in
try
raise (__ret.E (Printf.sprintf "%s, raza %s" (describe (d._embedded)) d.breed));
""
with
| __ret.E v -> v

type expr = Obj.t

type num = { mutable value : float; }

type var = { mutable name : string; }

type add = { mutable left : expr; mutable right : expr; }

type mul = { mutable left : expr; mutable right : expr; }

type neg = { mutable e : expr; }

type let_ = { mutable name : string; mutable val_ : expr; mutable body : expr; }

let rec isexpr (v : num) : unit =
  ()

let rec isexpr (v : var) : unit =
  ()

let rec isexpr (v : add) : unit =
  ()

let rec isexpr (v : mul) : unit =
  ()

let rec isexpr (v : neg) : unit =
  ()

let rec isexpr (v : let_) : unit =
  ()

let rec eval (e : expr) (env : (string, float) Hashtbl.t) : float =
let module __ret = struct exception E of float end in
try
(let v = (match e with | v -> v) in match v with
| num ->
raise (__ret.E (v.value))
| var ->
raise (__ret.E ((env).(v.name)))
| add ->
raise (__ret.E ((eval v.left env + eval v.right env)))
| mul ->
raise (__ret.E ((eval v.left env * eval v.right env)))
| neg ->
raise (__ret.E ((- eval v.e env)))
| let_ ->
let newenv = ref [||] in (
(for i = 0 to Array.length (env) - 1 do
        let k = i in
        let val_ = (env).(i) in
(!newenv).(k) = val_
done);
((!newenv).(v.name) = eval v.val_ env);
raise (__ret.E (eval v.body !newenv))));
raise (__ret.E (float_of_int (0)));
0.0
with
| __ret.E v -> v

type tree = Obj.t

type leaf = unit

type tnode = { mutable left : tree; mutable value : int; mutable right : tree; }

let rec istree (v : leaf) : unit =
  ()

let rec istree (v : tnode) : unit =
  ()

let rec insert (t : tree) (v : int) : tree =
let module __ret = struct exception E of tree end in
try
(let n = (match t with | v -> v) in match n with
| leaf ->
raise (__ret.E ({ left = Obj.magic (); value = v; right = Obj.magic () }))
| tnode ->
(if (v < n.value) then
raise (__ret.E ({ left = insert n.left v; value = n.value; right = n.right })) else
if (v > n.value) then
raise (__ret.E ({ left = n.left; value = n.value; right = insert n.right v })));
raise (__ret.E (n)));
raise (__ret.E (t));
Obj.magic ()
with
| __ret.E v -> v

let rec inorder (t : tree) : int array =
let module __ret = struct exception E of int array end in
try
(let n = (match t with | v -> v) in match n with
| leaf ->
raise (__ret.E (()))
| tnode ->
let result = ref [||] in (
(!result = Array.append !result inorder n.left);
(!result = Array.append !result [|n.value|]);
(!result = Array.append !result inorder n.right);
raise (__ret.E (!result))));
raise (__ret.E (()));
[||]
with
| __ret.E v -> v

type intlist = Obj.t

type nil = unit

type cons = { mutable head : int; mutable tail : intlist; }

let rec isintlist (v : nil) : unit =
  ()

let rec isintlist (v : cons) : unit =
  ()

let rec listlen (l : intlist) : int =
let module __ret = struct exception E of int end in
try
(let v = (match l with | v -> v) in match v with
| nil ->
raise (__ret.E (0))
| cons ->
raise (__ret.E ((1 + listlen v.tail))));
raise (__ret.E (0));
0
with
| __ret.E v -> v

let rec listsum (l : intlist) : int =
let module __ret = struct exception E of int end in
try
(let v = (match l with | v -> v) in match v with
| nil ->
raise (__ret.E (0))
| cons ->
raise (__ret.E ((v.head + listsum v.tail))));
raise (__ret.E (0));
0
with
| __ret.E v -> v

let rec sumslice (xs : int array) : int =
let module __ret = struct exception E of int end in
try
let total = ref 0 in (
(for i = 0 to Array.length (xs) - 1 do
      let _ = i in
      let x = (xs).(i) in
!total = (!total + x)
done);
raise (__ret.E (!total)));
0
with
| __ret.E v -> v

let rec reverseslice (xs : int array) : int array =
let module __ret = struct exception E of int array end in
try
let n = ref Array.length xs in (
let result = ref [||] in (
(for i = 0 to Array.length (xs) - 1 do
      let i = i in
      let x = (xs).(i) in
(!result).(((!n - 1) - i)) = x
done);
raise (__ret.E (!result))));
[||]
with
| __ret.E v -> v

let rec findindex (xs : int array) (target : int) : (int * bool) =
let module __ret = struct exception E of (int * bool) end in
try
(for i = 0 to Array.length (xs) - 1 do
      let i = i in
      let x = (xs).(i) in
if (x = target) then
raise (__ret.E ((i, true)))
done);
raise (__ret.E (((- 1), false)));
(0, false)
with
| __ret.E v -> v

let rec countwords (words : string array) : (string, int) Hashtbl.t =
let module __ret = struct exception E of (string, int) Hashtbl.t end in
try
let counts = ref [||] in (
(for i = 0 to Array.length (words) - 1 do
      let _ = i in
      let w = (words).(i) in
()
done);
raise (__ret.E (!counts)));
Hashtbl.create 16
with
| __ret.E v -> v

let rec lookupage (m : (string, int) Hashtbl.t) (name : string) : (int * bool) =
let module __ret = struct exception E of (int * bool) end in
try
let (age, ok) = (m).(name) in (
raise (__ret.E ((!age, !ok))));
(0, false)
with
| __ret.E v -> v

let rec mapint (xs : int array) (f) : int array =
let module __ret = struct exception E of int array end in
try
let result = ref [||] in (
(for i = 0 to Array.length (xs) - 1 do
      let i = i in
      let x = (xs).(i) in
(!result).(i) = f x
done);
raise (__ret.E (!result)));
[||]
with
| __ret.E v -> v

let rec filterint (xs : int array) (pred) : int array =
let module __ret = struct exception E of int array end in
try
let result = ref [||] in (
(for i = 0 to Array.length (xs) - 1 do
      let _ = i in
      let x = (xs).(i) in
if pred x then
!result = Array.append !result [|x|]
done);
raise (__ret.E (!result)));
[||]
with
| __ret.E v -> v

let rec foldleftint (xs : int array) (init : int) (f) : int =
let module __ret = struct exception E of int end in
try
let acc = ref init in (
(for i = 0 to Array.length (xs) - 1 do
      let _ = i in
      let x = (xs).(i) in
!acc = f !acc x
done);
raise (__ret.E (!acc)));
0
with
| __ret.E v -> v

let rec map (xs : 't array) (f) : 'u array =
let module __ret = struct exception E of 'u array end in
try
let result = ref [||] in (
(for i = 0 to Array.length (xs) - 1 do
      let i = i in
      let x = (xs).(i) in
(!result).(i) = f x
done);
raise (__ret.E (!result)));
[||]
with
| __ret.E v -> v

let rec filter (xs : 't array) (pred) : 't array =
let module __ret = struct exception E of 't array end in
try
let result = ref [||] in (
(for i = 0 to Array.length (xs) - 1 do
      let _ = i in
      let x = (xs).(i) in
if pred x then
!result = Array.append !result [|x|]
done);
raise (__ret.E (!result)));
[||]
with
| __ret.E v -> v

let rec fold (xs : 't array) (init : 'u) (f) : 'u =
let module __ret = struct exception E of 'u end in
try
let acc = ref init in (
(for i = 0 to Array.length (xs) - 1 do
      let _ = i in
      let x = (xs).(i) in
!acc = f !acc x
done);
raise (__ret.E (!acc)));
Obj.magic ()
with
| __ret.E v -> v

type pair = { mutable first : 'a; mutable second : 'b; }

let rec makepair (a : 'a) (b : 'b) : pair =
let module __ret = struct exception E of pair end in
try
raise (__ret.E ({ first = a; second = b }));
Obj.magic ()
with
| __ret.E v -> v

let rec makeadder (n : int) : Obj.t =
let module __ret = struct exception E of Obj.t end in
try
raise (__ret.E ((fun x -> 
raise (__ret.E ((x + n))))));
Obj.magic ()
with
| __ret.E v -> v

let rec makecounter () : Obj.t =
let module __ret = struct exception E of Obj.t end in
try
let count = ref 0 in (
raise (__ret.E ((fun () -> 
(!count := !!count + 1);
raise (__ret.E (!count))))));
Obj.magic ()
with
| __ret.E v -> v

let rec curry (f) : Obj.t =
let module __ret = struct exception E of Obj.t end in
try
raise (__ret.E ((fun a -> 
raise (__ret.E ((fun b -> 
raise (__ret.E (f a b))))))));
Obj.magic ()
with
| __ret.E v -> v

let rec compose (f) (g) : Obj.t =
let module __ret = struct exception E of Obj.t end in
try
raise (__ret.E ((fun x -> 
raise (__ret.E (f g x)))));
Obj.magic ()
with
| __ret.E v -> v

let rec factorial (n : int) : int =
let module __ret = struct exception E of int end in
try
(if (n <= 1) then
raise (__ret.E (1)));
raise (__ret.E ((n * factorial (n - 1))));
0
with
| __ret.E v -> v

let rec fibmemo () : Obj.t =
let module __ret = struct exception E of Obj.t end in
try
let cache = ref [|0; 1|] in (
let fib = ref Obj.magic () in (
(!fib = (fun n -> 
(let (v, ok) = (!cache).(n) in if ok then
raise (__ret.E (v)));
let r = (fib (n - 1) + fib (n - 2)) in (
((!cache).(n) = r);
raise (__ret.E (r)))));
raise (__ret.E (!fib))));
Obj.magic ()
with
| __ret.E v -> v

let rec iseven (n : int) : bool =
let module __ret = struct exception E of bool end in
try
(if (n = 0) then
raise (__ret.E (true)));
raise (__ret.E (isodd (n - 1)));
false
with
| __ret.E v -> v

let rec isodd (n : int) : bool =
let module __ret = struct exception E of bool end in
try
(if (n = 0) then
raise (__ret.E (false)));
raise (__ret.E (iseven (n - 1)));
false
with
| __ret.E v -> v

let rec divide (a : float) (b : float) : (float * Obj.t) =
let module __ret = struct exception E of (float * Obj.t) end in
try
(if (b = 0) then
raise (__ret.E ((float_of_int (0), "división por cero"))));
raise (__ret.E (((a /. b), ())));
(0.0, Obj.magic ())
with
| __ret.E v -> v

let rec parsepositive (s : string) : (int * Obj.t) =
let module __ret = struct exception E of (int * Obj.t) end in
try
let n = ref 0 in (
let (_, err) = (sscanf) s "%d" (ref !n) in (
(if (!err <> ()) then
raise (__ret.E ((0, Printf.sprintf "no es número: %q" s))));
(if (!n < 0) then
raise (__ret.E ((0, Printf.sprintf "negativo: %d" !n))));
raise (__ret.E ((!n, ())))));
(0, Obj.magic ())
with
| __ret.E v -> v

let rec minmax (xs : int array) : (int * int) =
let module __ret = struct exception E of (int * int) end in
try
(if (Array.length xs = 0) then
raise (__ret.E ((0, 0))));
let mn = ref (xs).(0) in let mx = ref (xs).(0) in (
(for i = 0 to Array.length (Array.sub xs 0 (Array.length xs)) - 1 do
      let _ = i in
      let x = (Array.sub xs 0 (Array.length xs)).(i) in
(if (x < !mn) then
!mn = x);
if (x > !mx) then
!mx = x
done);
raise (__ret.E ((!mn, !mx))));
(0, 0)
with
| __ret.E v -> v

let rec incrementref (n) : unit =
()

let rec swapints (a) (b) : unit =
!(a) = !(b); !(b) = !(a)

type stack = { mutable items : int array; }

let rec newstack () : Obj.t =
let module __ret = struct exception E of Obj.t end in
try
raise (__ret.E ((ref Obj.magic ())));
Obj.magic ()
with
| __ret.E v -> v

let rec push (s) (v : int) : unit =
items s = Array.append items s [|v|]

let rec pop (s) : (int * bool) =
let module __ret = struct exception E of (int * bool) end in
try
(if (Array.length items s = 0) then
raise (__ret.E ((0, false))));
let last = ref (Array.length items s - 1) in (
let v = ref (items s).(!last) in (
(items s = Array.sub items s 0 (Array.length items s));
raise (__ret.E ((!v, true)))));
(0, false)
with
| __ret.E v -> v

let rec len (s) : int =
let module __ret = struct exception E of int end in
try
raise (__ret.E (Array.length items s));
0
with
| __ret.E v -> v

let rec classify (n : int) : string =
let module __ret = struct exception E of string end in
try
""
with
| __ret.E v -> v

let rec daykind (w : int) : string =
let module __ret = struct exception E of string end in
try
""
with
| __ret.E v -> v

let rec gcd (a : int) (b : int) : int =
let module __ret = struct exception E of int end in
try
(while (!b <> 0) do
!a = !b; !b = (!a mod !b)
   done);
raise (__ret.E (!a));
0
with
| __ret.E v -> v

let rec power (base : int) (exp : int) : int =
let module __ret = struct exception E of int end in
try
let result = ref 1 in (
(let i = ref 0 in while (!i < exp) do
(!result = (!result * base));
!i := !!i + 1
   done);
raise (__ret.E (!result)));
0
with
| __ret.E v -> v

let rec safedivide (a : int) (b : int) : (int * bool) =
let module __ret = struct exception E of (int * bool) end in
try
(if (b = 0) then
raise (__ret.E ((0, false))));
raise (__ret.E (((a / b), true)));
(0, false)
with
| __ret.E v -> v

let rec firsteven (xs : int array) : (int * bool) =
let module __ret = struct exception E of (int * bool) end in
try
(for i = 0 to Array.length (xs) - 1 do
      let _ = i in
      let x = (xs).(i) in
if ((x mod 2) = 0) then
raise (__ret.E ((x, true)))
done);
raise (__ret.E ((0, false)));
(0, false)
with
| __ret.E v -> v

let rec normalizespaces (s : string) : string =
let module __ret = struct exception E of string end in
try
raise (__ret.E (String.concat " " (Array.to_list [||])));
""
with
| __ret.E v -> v

let rec ispalindrome (s : string) : bool =
let module __ret = struct exception E of bool end in
try
let n = ref Array.length s in (
(let i = ref 0 in while (!i < (!n / 2)) do
(if ((s).(!i) <> (s).(((!n - 1) - !i))) then
raise (__ret.E (false)));
!i := !!i + 1
   done);
raise (__ret.E (true)));
false
with
| __ret.E v -> v

let rec main () : unit =
let c = ref 100 in (
((printf) "%.1f°C = %.1f°F\n" !c celsiustofahrenheit !c);
let today = ref wednesday in (
((printf) "hoy: %s (finde? %v)\n" !today isweekend !today);
let p1 = ref { x = 1; y = 2 } in (
let p2 = ref (translate !p1) 3 4 in (
((println) "translated:" !p2 "dist²:" (distancesquared !p1) !p2);
let r = ref { topleft = Obj.magic (); bottomright = Obj.magic () } in (
((printf) "rect area = %.1f\n" (area !r));
let rex = ref { animal = { name = "Rex"; species = "Canis" }; breed = "Labrador" } in (
((println) (fulldescription (!rex._embedded)));
let expr = ref { name = "x"; val_ = Obj.magic (); body = { left = { left = Obj.magic (); right = Obj.magic () }; right = { e = Obj.magic () } } } in (
((println) "eval:" eval !expr ());
let t = ref Obj.magic () in (
(for i = 0 to Array.length ([|5; 3; 8; 1; 4; 7; 9|]) - 1 do
      let _ = i in
      let v = ([|5; 3; 8; 1; 4; 7; 9|]).(i) in
!t = insert !t !v
done);
((println) "inorder:" inorder !t);
let list = ref { head = 1; tail = { head = 2; tail = { head = 3; tail = Obj.magic () } } } in (
((println) "len:" listlen !list "sum:" listsum !list);
let nums = ref [|3; 1; 4; 1; 5; 9; 2; 6|] in (
((println) "sum:" sumslice !nums "reversed:" reverseslice !nums);
(let (idx, ok) = findindex !nums 5 in if !ok then
(println) "5 encontrado en índice" !idx);
let wc = ref countwords [|"foo"; "bar"; "foo"; "baz"; "bar"; "foo"|] in (
let keys = ref [||] in (
(for i = 0 to Array.length (!wc) - 1 do
      let k = i in
      let _ = (!wc).(i) in
!keys = Array.append !keys [|k|]
done);
(());
(for i = 0 to Array.length (!keys) - 1 do
      let _ = i in
      let k = (!keys).(i) in
(printf) "  %s: %d\n" k (!wc).(k)
done);
let ages = ref [|30; 25|] in (
(let (age, ok) = lookupage !ages "Alice" in if !ok then
(println) "Alice:" !age);
let doubled = ref mapint !nums (fun x -> 
(!x * 2)) in (
let evens = ref filterint !nums (fun x -> 
((!x mod 2) = 0)) in (
let total = ref foldleftint !nums 0 (fun a b -> 
(!a + !b)) in (
((println) "doubled:" !doubled "evens:" !evens "total:" !total);
let words = ref [|"hola"; "mundo"; "ocaml"|] in (
let lens = ref map !words (fun s -> 
Array.length s) in (
((println) "longitudes:" !lens);
let pair = ref makepair "edad" 30 in (
((printf) "%+v\n" !pair);
let add3 = ref makeadder 3 in (
((println) "add3(10) =" add3 10);
let tick = ref makecounter in (
((println) tick tick tick);
let add = ref curry (fun a b -> 
(!a + !b)) in (
((println) "curry:" (add 5) 7);
let inc = ref (fun x -> 
(!x + 1)) in (
let dbl = ref (fun x -> 
(!x * 2)) in (
let incthendbl = ref compose !dbl !inc in (
((println) "compose:" incthendbl 4);
((println) "5! =" factorial 5);
let fib = ref fibmemo in (
let fibs = ref [||] in (
(for i = 0 to Array.length (!fibs) - 1 do
      let i = i in
      let _ = (!fibs).(i) in
(!fibs).(i) = fib i
done);
((println) "fib:" !fibs);
((println) "even(10)?" iseven 10 "odd(7)?" isodd 7);
(let (q, err) = divide 22 7 in if (!err = ()) then
(printf) "22/7 ≈ %.4f\n" !q);
(let (_, err) = divide 1 0 in if (!err <> ()) then
(println) "err esperado:" !err);
(let (n, err) = parsepositive "42" in if (!err = ()) then
(println) "parsed:" !n);
let (mn, mx) = minmax !nums in (
((println) "min:" !mn "max:" !mx);
let x = ref 10 in (
(incrementref (ref !x));
((println) "x =" !x);
let a = ref 1 in let b = ref 2 in (
(swapints (ref !a) (ref !b));
((println) "swap:" !a !b);
let st = ref newstack in (
(for i = 0 to Array.length ([|1; 2; 3; 4|]) - 1 do
      let _ = i in
      let v = ([|1; 2; 3; 4|]).(i) in
(push !st) !v
done);
((print) "stack pop: ");
(while ((len !st) > 0) do
let (v, ok) = (pop !st) in if !ok then
(print) !v " "
   done);
((println));
(for i = 0 to Array.length ([|(- 5); 0; 7; 50; 500|]) - 1 do
      let _ = i in
      let n = ([|(- 5); 0; 7; 50; 500|]).(i) in
(printf) "%d → %s; " !n classify !n
done);
((println));
((println) daykind friday daykind sunday daykind tuesday);
((println) "gcd(48,18) =" gcd 48 18);
((println) "2^10 =" power 2 10);
(let (q, ok) = safedivide 10 3 in if !ok then
(println) "10/3 =" !q);
(let (v, ok) = firsteven [|1; 3; 5; 8; 9|] in if !ok then
(println) "primer par:" !v);
((printf) "[%s]\n" normalizespaces "   hola    mundo  ");
for i = 0 to Array.length ([|"anilina"; "hola"; "reconocer"; "abba"|]) - 1 do
      let _ = i in
      let w = ([|"anilina"; "hola"; "reconocer"; "abba"|]).(i) in
(printf) "%s palíndromo? %v\n" w ispalindrome w
done)))))))))))))))))))))))))))))))

let () = main ()