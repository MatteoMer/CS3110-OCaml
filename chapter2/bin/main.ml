(** Exercises: 
    1.  int: 42
        string: CS3310 **)

(** 2. **)
let () = print_endline "Hello, World!"

let () = string_of_int (42 * 10) |> print_endline

let () = string_of_float (3.14 /. 2.0) |> print_endline

let () = string_of_float (4.2 ** 7.) |> print_endline

(** 3. **)
let () = assert(42==42)
let () = assert("hi"=="hi")
let () = assert("hi"="hi")


(** 3. **)
(** let () = assert(2110=3110) **)

let double x = x * 2

let () = assert(double 7=14)
let () = assert(double 7!=15)

let cube (x: float) = x ** 3.

let () = assert(cube 3.=27.)

let sign x = if x = 0 then 0 else if x > 0 then 1 else -1

let () = assert(sign 42=1)
let () = assert(sign 0=0)
let () = assert(sign (-42)=(-1))

let area (r:float) = 
    let pi = (2.0 *. asin 1.0) in
    pi *. (r ** 2.)


let () = string_of_float (area 5.) |> print_endline


let root_mean_square x y = (x ** 2.) +. (y ** 2.) |> Float.sqrt

let () = string_of_float (root_mean_square 2. 3.) |> print_endline

let valid_date (d:int) (m: string) = 

    let max_days m = if m="Jan" then 31 else if m="Feb" then 28 else if m="Mar" then 31 
    else if m="Apr" then 30 else if m="May" then 31 else if m="Jun" then 30
    else if m="Jul" then 31 else if m="Aug" then 31 else if m="Sept" then 30
    else if m="Oct" then 31 else if m="Nov" then 30 else if m="Dec" then 31 else -1
    in

    max_days m >= d

let _ = valid_date 60 "Dec"
let () = assert(valid_date 50 "Dec"=false)
let () = assert(valid_date 30 "Dec"=true)
let () = assert(valid_date 30 "Feb"=false)
let () = assert(valid_date 28 "Feb"=true)

let rec fib x = 
    if x == 1 then 1 else if x == 2 then 1 else fib (x-1) + fib (x-2)

let () = assert(fib 4=3)
let () = assert(fib 7=13)


let divide (numerator:float) (denominator:float) = 
    numerator /. denominator

