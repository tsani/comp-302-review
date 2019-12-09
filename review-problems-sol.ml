type 'a mylist = Nil | Cons of 'a * 'a mylist

let rec of_list = function
| [] -> Nil
| x::xs -> Cons (x, (of_list xs))

let rec append l1 l2 = match l1 with
| Nil -> l2
| Cons(x,xs) -> Cons(x, (append xs l2))

let rec map f l = match l with
| Nil -> Nil
| Cons(x,xs) -> let r = f x in Cons(r, (map f xs))

let map2 f l1 l2 =
  List.fold_right (fun x k -> fun (y :: ys) -> f x y :: k ys) l1 (fun [] -> []) l2

let length l =
   let rec length' acc = function
   | Nil -> acc
   | Cons(x,xs) -> length' (acc+1) xs
   in
  length' 0 l

(* THM: length (map f l) = length l

   Base case: For any (f : 'a -> 'b), length (map f Nil) = length Nil
   length(map f Nil)=length (Nil) = length Nil

   Now let l= Cons(x,xs). IH: length (map f xs)= length xs

   length (map f Cons(x,xs))= length Cons(f x, map f xs) = 1 + length (map f xs) 
   = 1+ length xs (IH) = length(x::xs)

   THM 2 : append (map f l1) (map f l2) = map f (append l1 l2)

   Base case: for any (f : 'a -> 'b) and any (l2 : 'a list), 
   append (map f Nil) (map f l2) = map f (append Nil l2)

   append (map f Nil) (map f l2) = append Nil (map f l2) = map f l2
   map f (append Nil l2) = map f l2 

   Now let l1 = Cons(x,xs). IH: append (map f xs) (map f l2)=map f (append xs l2)

   append (map f Cons(x,xs)) (map f l2) = append Cons(f x, map f xs) (map f l2)
   =Cons(f x, append (map f xs) (map f l2)) = Cons(f x, map f (append xs l2))
   =map f Cons(x, append xs l2)=map f (append Cons(x,xs) l2)
   *)



let rec fold_right f l acc = match l with
  | Nil -> acc
  | Cons(x,xs) -> f x (fold_right f xs acc)

let map' f l =
    fold_right (fun x acc -> Cons(f x, acc)) l Nil

(*  Thm: map' f l= map f l
    Base case: map' f Nil = fold_right (fun x y -> Cons((f x),y) l Nil
    = Nil = map f Nil

    Now let l= Cons(x,xs). IH: map' f xs = map f xs

    map' f Cons(x,xs)= fold_right (fun x y -> Cons((f x),y) Cons(x,xs) Nil
    = Cons(f x, (fold_right (fun x y -> Cons((f x),y) xs Nil))
    =Cons( f x, map' f xs)= Cons(f x, map f xs) (Using IH)
    =map f Cons(x,xs)
    *)



let rec fold_left f acc l = match l with
  | Nil -> acc
  | Cons(x,xs) -> fold_left f (f acc x) xs

let fold_left' f e l =
  fold_right (fun a b -> f b a) (fold_right (fun a b -> append b (Cons(a, Nil))) l Nil)

let rev l =
  fold_left (fun acc x-> Cons(x,acc)) Nil l

let rev' l =
  fold_right (fun a b -> append b (Cons(a, Nil))) l Nil

let for_all p l =
    fold_right (fun x acc -> acc && p x) l true

let exists p l =
    fold_right (fun x acc -> acc || p x) l false

let for_all' p l =
   not (exists (fun x -> not (p x)) l)

let exists' p l =
    not (for_all (fun x -> not (p x)) l)

let sum l =
    fold_right (+) l 0

let rec combine l1 l2 = match l1, l2 with
| Nil, Nil -> Nil
| Cons(x,xs), Cons(y,ys) -> Cons((x,y), combine xs ys)
| _ -> failwith "lengths mismatched"

let map2 f l1 l2 =
    let tup = combine l1 l2 in
    map (fun (x,y) -> f x y) tup
    
let rec pairs l = match l with 
  | Nil -> Nil
  |Cons(x,Nil)-> Nil
  |Cons(x,Cons(y,xs))-> Cons((x,y),pairs Cons(y,xs))

(* Lazy Programming *)
module Lazy = struct
  type 'a susp = Susp of (unit -> 'a)
  let delay f = Susp f
  let force (Susp f) = f ()

  (* Usual definition of streams. *)
  type 'a str =
    { hd : 'a
    ; tl : 'a str susp
    }

  (* REMARK: I start from zero *)
  let rec nth n s =
    if n = 0 then s.hd else nth (n-1) (force s.tl)

  let rec fib : int str =
    let rec go n m =
      {hd = n;
       tl = Susp (fun () -> go (n+m) n)}
    in
    go 1 0

  let rec seq f =
    let rec go n =
      {hd = f n;
       tl = Susp (fun () -> go (n+1))}
    in
    go 0

  let wallis_1 =
    let rec f n =
      if n = 0 then 4.0 /. 3.0
      else
        let x = (float_of_int) (4*(n+1)*(n+1)) in
        let y = (float_of_int) (4*(n+1)*(n+1) - 1) in
        (x /. y) *. (f (n-1))
    in
    seq f

  (* REMARK: index starts from 1 *)
  let wallis_2 =
    let rec go n w_n =
      let x = (float_of_int) (4*(n+1)*(n+1)) in
      let y = (float_of_int) (4*(n+1)*(n+1) - 1) in
      let a_n = x /. y in
      {
        hd = w_n;
        tl = Susp (fun () -> go (n+1) (a_n *. w_n))
      }
    in
    go 1 (4.0 /. 3.0)

  let superc m n =
    let rec fact x = match x with
      | 0 -> 1
      | d -> d * (fact (d-1))
    in
    (fact (2*m))*(fact (2*n)) / ((fact (m+n))*(fact m)*(fact n))

  let supercatalan =
    let rec get_col m k =
      {
        hd = superc m k ;
        tl = Susp (fun () -> get_col (m+1) k)
      }
    in
    let cols = get_col 0 in
    let rec get_supcat n =
      {
        hd = cols n ;
        tl = Susp (fun () -> get_supcat (n+1))
      }
    in
    get_supcat 0

end
