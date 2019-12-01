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

let rec fold_right f l acc = match l with
  | Nil -> acc
  | Cons(x,xs) -> f x (fold_right f xs acc)

let rec fold_left f acc l = match l with
  | Nil -> acc
  | Cons(x,xs) -> fold_left f (f acc x) xs

let rec rev l = 
  fold_left (fun acc x-> Cons(x,acc)) Nil l
  
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
                   
