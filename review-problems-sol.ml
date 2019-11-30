type 'a mylist = Nil | Cons of 'a * 'a mylist

let rec of_list = function
| [] -> Nil
| x::xs -> Cons (x, (of_list xs))

let rec append l1 l2 = match l1 with
| Nil -> l2
| Cons(x,xs) -> Cons(x, (append xs l2))

let rec map f l = 
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

let fold_left' f acc l = 
  let reverse l
  fold_right (fun a b -> f b a) acc (rev l)
  (* fun a b -> f b a switches accumulator and element *)
