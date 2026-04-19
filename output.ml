let decimalLit : Obj.t = ()

let vBool : bool = ()

let untypedInt : Obj.t = ()

type weekday = int

let Sunday : weekday = ()

type celsius = float

type fahrenheit = float

type temp = celsius

type mystring = string

type point = { mutable X : int; mutable Y : int; }

type empty = {  }

type tagged = { mutable Name : string; mutable Age : int; }

type embedded = { mutable _embedded : tagged; mutable _embedded : Obj.t; mutable Bonus : int; }

type node = { mutable Value : int; mutable Next : Obj.t; mutable Prev : Obj.t; }

type kitchen = { mutable B : bool; mutable I : int; mutable F : float; mutable C : complex128; mutable S : string; mutable R : int; mutable Byt : int; mutable Arr : int array; mutable Sl : int array; mutable Mp : (string, int) Hashtbl.t; mutable Sub : point; mutable Ptr : Obj.t; mutable Fn : Obj.t; }

let arrFixed : Obj.t = ()

let sliceLit : Obj.t = ()

let mapLit : Obj.t = ()

let pInt : Obj.t = ()

type unaryop = Obj.t