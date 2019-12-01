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

let rec append l1 l2 = match l1 with
| Nil -> l2
| Cons(x,xs) -> Cons(x, (append xs l2))

let rec fold_right f l acc = match l with
  | Nil -> acc
  | Cons(x,xs) -> f x (fold_right f xs acc)

let fold_left' f e l = 
	fold_right (fun a b -> f b a) (fold_right (fun a b -> append b (Cons(a, Nil))) l Nil)

let map' f l = 
  	fold_right (fun x acc -> Cons(f x, acc)) l Nil

let rev' l =
	fold_right (fun a b -> append b (Cons(a, Nil))) l Nil

let for_all p l = 
  	fold_right (fun x acc -> acc && p x) l true

let exists p l =
  	fold_right (fun x acc -> acc || p x) l false
  
let for_all' p l = 
 	not (exists (fun x -> not p) l)

let exists' p l = 
  	not (for_all (fun x -> not p) l)

let sum l = 
  	fold_right (+) l 0
   
let rec combine l1 l2 = match l1, l2 with 
| Nil, Nil -> Nil
| Cons(x,xs), Cons(y,ys) -> Cons((x,y), combine xs ys)

let map2 f l1 l2 = 
  	let tup = combine l1 l2 in
  	map (fun (x,y) -> f x y) tup
