(* This is the master list of practice problems, by looking at
   everything done in class, and thinking of potential extensions to
   class problems.
   
   Each problem has a ranking with stars.
   - Problems ranked 1 or 2 stars, you should be able to do.
   - Problems ranked 3 or more stars are challenging, and probably
     beyond what you would see on a test.

  DO THESE PROBLEMS ON PAPER.
  Do not use LearnOCaml!

  On the exam, you do not have a compiler to check your types, and you
  will lose marks for type errors, bad syntax, etc.

  Problems for each topic are in their own module.
 *)

module Basics = struct (* datatypes, pattern maching, and HOFs *)

  (* Define our own list type.  Now we will implement many functions
     on lists using our own type, in order to familiarize ourselves with
     pattern matching syntax.  *)
  type 'a mylist = Nil | Cons of 'a * 'a mylist

  (* of_list : 'a list -> 'a mylist *)
  (* Convert an ordinary OCaml list into our custom list type.
     This will allow you to test your programs using the nice OCaml
     list syntax instead of writing `Cons` a million times.

     Rank: *
   *)
  let of_list l = assert false

  (* append : 'a mylist -> 'a mylist -> 'a mylist
     Concatenate two lists together.
       append (of_list [1;2;3]) (of_list [4;5;6])
       = of_list [1;2;3;4;5;6]

     Rank: *
   *)
  let append l1 l2 = assert false

  (* Prove, using induction, the following theorem.

     THEOREM. (append is associative)
     For any 'a list l1, l2 and l3,
     append l1 (append l2 l3) = append (append l1 l2) l3

     Hint:
     There are lots of lists. Which one should you do induction on? In
     general, you should do induction on the object that drives the
     computation. In the case of append, you defined it by pattern
     matching and recursion on the first argument, so you should pick
     the first list here.

     Rank: *
   *)

  (* map : ('a -> 'b) -> 'a mylist -> 'b mylist *)
  (* Mimics the List.map function.
     It applies the function `f` to each element of the list.

     Rank: *
   *)
  let map f l = assert false

  (* length : 'a mylist -> int
     Mimics the List.length function.
     It computes the length of the list.

     Rank: *
   *)
  let length l = assert false

  (* Prove, using induction, the following theorems.

     THEOREM 1.
     For any (f : 'a -> 'b) and any (l : 'a list)
     length (map f l) = length l

     THEOREM 2.
     For any (f : 'a -> 'b) and any l1 and l2 of type 'a list
     append (map f l1) (map f l2) = map f (append l1 l2)

     Rank: *
   *)

  (* fold_right : ('a -> 'b -> 'b) -> 'a mylist -> 'b -> 'b *)
  (* Mimics the List.fold_right functions.
     `fold_right f l e` eliminates a list into a type 'b of your
     choosing by replacing Nil with `e` and replacing Cons with `f`.

     Rank: *
   *)
  let fold_right f l e = assert false

  (* According to the description above, `fold_right f l e` takes a
     list
     l = `Cons (x1, (Cons (x2, Cons (x3, Nil))))` for example
     and computes
     `f x1 (f x2 (f x2 e))`
     Notice that the applications of `f` are nested to the _right_.

     What if we wanted the opposite nesting?
     f (f (f e x1) x2) x3

     This this is a left fold, instead of a right fold.
   *)

  (* fold_left : ('b -> 'a -> 'b) -> 'b -> 'a mylist -> 'b *)
  (* First, implement fold_left using recursion and pattern
     matching.
     
     Rank: *
   *)
  let fold_left f e l = assert false

  (* Theoretically, any recursive function you can write for lists
     can be implemented with fold_right, so in particular, one should
     be able to implement fold_left in terms of fold_right.
   *)
  (* fold_left' : ('b -> 'a -> 'b) -> 'b -> 'a mylist -> 'b *)
  (* Implement fold_left' in terms of fold_right, without using
     recursion or pattern matching at all.
     
     Rank: ***
   *)
  let fold_left' f e l = assert false

  (* Now let's implement some other, less complicated higher-order
     functions using fold_right. *)

  (* map' : ('a -> 'b) -> 'a mylist -> 'b mylist *)
  (* Implement map' using fold_right so that it does the same thing as
     map.
     
     Rank: **
   *)
  let map' f l = assert false

  (* Prove, using induction on lists, the following.

     THEOREM:
     For any (f : 'a -> 'b) and any (l : 'a mylist),
     map' f l = map f l

     Rank: *
   *)

  (* The OCaml List module defines
     List.map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> c 'list

     Implement
     map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
     using `map`.
     Assume the input lists have the same length (no need to check
     this).

     Hint: you can use `map` twice!

     Rank: ***
   *)
  let map2 f l1 l2 = assert false

  (* Can you implement map2' in terms of only one *single* call to
     fold_right?
     Assume the input lists have the same length (no need to check
     this).

     Simplification in case you get stuck:
     you are allowed to reverse l2
     (you will need to implement rev).

     Rank: **** (if you use rev, too)
     Rank: ***** (if you only use fold_right; no other recursive list functions allowed!)
     
     Hint for if you only allow using fold_right:
     use the fold to build up a *function* that ultimately acceps l2 as an input.
   *)
  let map2' f l1 l2 =
    assert false

  (* It's easy to implement reverse using fold_left.
     rev : 'a mylist -> 'a mylist
     Reverses a list.

     Implement it using fold_left.
     Rank: **
   *)
  let rev l =
    assert false

  (* On the other hand it is much more challenging to do it in terms
     of fold_right.
     rev' : 'a mylist -> 'a mylist
     Reverses a list.

     Implement it using fold_right.
     Rank: ****
   *)

  (* for_all : ('a -> bool) -> 'a mylist -> bool *)
  (* `for_all p l` = true
     if and only if every element of `l` satisfies `p`.
     That is, for all `x` in `l`, we have `p x` is true.

     Hint: start by thinking about whether `for_all p Nil` should be
     true or false.

     Implement this function in terms of fold_right.

     Rank: **
   *)
  let for_all p l = assert false

  (* exists : ('a -> bool) -> 'a mylist -> bool *)
  (* `exists p l` is true
     if and only if
     there is some x in l such that p x is true.

     Hint: start by thinking about whether `exists p Nil` should be
     true or false.

     Rank: **
   *)
  let exists p l = assert false

  (* By the way, in formal logic we can make the following observation:

     forall x, P x
     is equivalent to
     ~ exists x such that ~(P x)

     (I am writing ~ for logical negation.)

     Seems reasonable, right?
     If I can't find an x to make P x false, then it must mean that P
     x is true for all x.

     Using this observation, implement for_all' in terms of exists and
     exists' in terms of for_all.

     Rank: **
   *)
  let for_all' p l = assert false
  let exists' p l = assert false

  (* Prove, using induction, the following.

     THEOREM:
     for any (p : 'a -> bool) and any (l : 'a mylist),
     for_all p l = for_all' p l

     Hints:
     - You do not need to generalize the theorem.
     - In the step case, consider subcases for whether p x is true or
       false.
     - You might need de Morgan's laws:
       1. not (a || b) = not a && not b
       2. not (a && b) = not a || not b

     Rank: **
   *) 

  (* sum : int mylist -> int *)
  (* Adds up all the elements of a list.
  
     Implement this function in terms of fold_right.

     Rank: *
   *)
  let sum l = assert false

  (* Prove, using induction, the following theorem.

     THEOREM.
     For any l : int mylist
     sum l = fold_left (+) 0 l

     This theorem must be generalized.

     THEOREM.
     For any acc : int and any l : int mylist,
     sum l + acc = fold_left (+) acc l

     After proving the generalized theorem, prove the original theorem
     in 1 or two lines by invoking the generalized theorem.
   *)
end

module Induction = struct
end
