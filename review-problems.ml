(* Author: Jacob Thomas Errington

   This is the master list of practice problems.
   I made it by looking at everything done in class, and thinking of
   potential extensions to class problems.

   DO THESE PROBLEMS ON PAPER.
   Do not use LearnOCaml!

   On the exam, you do not have a compiler to check your types, and you
   will lose marks for type errors, bad syntax, etc.

   Of course, to *check* your work, you can type up your solution in
   LearnOCaml and test it. Better yet, compare your solution with a
   classmate and learn from each other!

   Each problem has a ranking with stars.
   - Problems ranked 1 or 2 stars, you should be able to do.
   - Problems ranked 3 or more stars are challenging, and probably
     beyond what you would see on a test.

   It's probably a good idea to at least *attempt* 3-star problems,
   even if you don't feel super comfortable with the material, because
   they will hopefully push the boundaries of your knowledge. But
   don't feel bad if you can't solve
   them without hints from your
   friends.

   One more remark about the stars: they don't reflect overall
   difficulty of the question. They reflect the difficulty of the
   question among that *type* of question.
   For example, there are 1-star continuation questions as well as
   1-star ordinary recursion questions.
   They are not equally difficult!
   The continuation question may be harder than the recursion
   question, but _for a continuation question_ it's 1-star.

   Problems for each topic are in their own module.
 *)

(* datatypes, pattern maching, and HOFs, continuations *)
module Functions = struct

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
  let rec of_list l = match l with
      [] -> Nil
    |x :: xs -> Cons (x,of_list xs)

  (* append : 'a mylist -> 'a mylist -> 'a mylist
     Concatenate two lists together.
       append (of_list [1;2;3]) (of_list [4;5;6])
       = of_list [1;2;3;4;5;6]

     Rank: *
   *)
  let rec append l1 l2 = match l1 with
    |Cons (x,xs) -> Cons (x,append xs l2)
    | Nil -> l2

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
  let rec map f l = match l with
     Nil -> Nil
  | Cons (x,xs) -> Cons (f x ,map f xs)

  (* length : 'a mylist -> int
     Mimics the List.length function.
     It computes the length of the list.

     Rank: *
   *)
  let rec length l = match l with
      Nil -> 0
    |Cons (x,xs) -> 1+ (length xs)

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
  let rec fold_right f l e = match l with
      Cons (x,xs) -> f x (fold_right f xs e)
    | Nil -> e

  (* According to the description above, `fold_right f l e` takes a
     list
     l = `Cons (x1, (Cons (x2, Cons (x3, Nil))))` for example
     and computes
     `f x1 (f x2 (f x3 e))`
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
  let rec fold_left f e l = match l with
      Cons (x,xs) -> fold_left f (f e x) xs
    |Nil -> e

  (* Theoretically, any recursive function you can write for lists
     can be implemented with fold_right, so in particular, one should
     be able to implement fold_left in terms of fold_right.
   *)
  (* fold_left' : ('b -> 'a -> 'b) -> 'b -> 'a mylist -> 'b *)
  (* Implement fold_left' in terms of fold_right, without using
     recursion or pattern matching at all.
     Note: reversing the list doesn't help.

     Rank: ***

   *)
  let fold_left' (f:'a->'b->'a) (e:'a) (l:'b mylist) = (fold_right (fun x y -> (fun z -> y (f z x))) l (fun w -> w)) e

  (* Now let's implement some other, less complicated higher-order
     functions using fold_right. *)

  (* map' : ('a -> 'b) -> 'a mylist -> 'b mylist *)
  (* Implement map' using fold_right so that it does the same thing as
     map.

     Rank: **
   *)
  let map' f l = fold_right (fun x y -> Cons ((f x), y)) l Nil

  (* Prove, using induction on lists, the following.

     THEOREM:
     For any (f : 'a -> 'b) and any (l : 'a mylist),
     map' f l = map f l

     Rank: *
   *)

  (* Implement the function
     combine : 'a list -> 'b list -> ('a * 'b) list
     which pairs up the elements of the input lists.
     Assume that the input lists have the same length.

     Implement this as a recursive function.
     Rank: *
  *)
  exception Invalid_argument of string
  let rec combine l1 l2 = match l1, l2 with
      Cons(x,xs),Cons(y,ys) -> Cons ((x,y),combine xs ys)
    | Nil, Nil -> Nil
    | _ -> raise (Invalid_argument "List length don't match")


  (* The OCaml List module defines
     List.map2 : ('a -> 'b -> 'c) -> 'a mylist -> 'b mylist -> 'c mylist

     Implement
     map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
     using `combine` and `map`
     such that
     map2 f [x1;x2;...] [y1;y2;...] = [f x1 y1; f x2 y2; ...]

     Assume the input lists have the same length (no need to check
     this).

     Rank: ***
   *)
  let map2 f l1 l2 =
    let aux_l = combine l1 l2 in
    let aux_f (x,y) = f x y in
    map aux_f aux_l


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
  let map2' f l1 l2 = (fold_right
                         (fun x k -> fun z -> match z with
                            | Cons (y,ys) , xs -> ys , Cons ((f x y), xs)
                            |  _  ->  raise (Invalid_argument "List length don't match")
                         )
                         l1
                         (fun w -> w)
                      ) (l2,Nil)

  let map2_improved f l1 l2 = (fold_right
                                 (fun x k -> fun z -> match z with
                                    | Cons (y,ys) -> Cons (f x y, k ys)
                                    | _ -> raise (Invalid_argument "List lengths don't match")
                                 )
                                 l1
                                 (fun w -> w)
                              ) l2

  (* It's easy to implement reverse using fold_left.
     rev : 'a mylist -> 'a mylist
     Reverses a list.

     Implement it using fold_left.
     Rank: **
   *)
  let rev l = fold_left (fun x y -> Cons (y,x)) Nil l

  (* On the other hand it is much more challenging to do it in terms
     of fold_right.
     rev' : 'a mylist -> 'a mylist
     Reverses a list.

     Implement it using fold_right.
     Rank: ****
  *)

  let rev' l = (fold_right
                  (fun x k -> fun z -> k (Cons (x,z) ))
                  l
                  (fun w -> w)
               ) Nil

  (* for_all : ('a -> bool) -> 'a mylist -> bool *)
  (* `for_all p l` = true
     if and only if every element of `l` satisfies `p`.
     That is, for all `x` in `l`, we have `p x` is true.

     Hint: start by thinking about whether `for_all p Nil` should be
     true or false.

     Implement this function in terms of fold_right.

     Rank: **
   *)
  let for_all p l = fold_right (fun x y -> y && p x ) l true
  (* exists : ('a -> bool) -> 'a mylist -> bool *)
  (* `exists p l` is true
     if and only if
     there is some x in l such that p x is true.

     Hint: start by thinking about whether `exists p Nil` should be
     true or false.

     Rank: **
   *)
  let exists p l = fold_right (fun x y -> y || p x) l false

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
  let for_all' p l =
    let p' x = not (p x) in
    not (exists p' l)

  let exists' p l =
    let p' x = not (p x) in
    not (for_all p' l)

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
  let sum l = fold_right (+) l 0

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

     Rank: **
   *)

  (* You'll notice that the only think *specific* to addition that we
     needed in the previous theorem was associativity. This suggests a
     natural generalization of the previous theorem.

     THEOREM.
     Suppose f : 'a -> 'a -> 'a is an associative function, i.e.
     for any x, y, z of type 'a, we have f (f x y) z = f x (f y z)
     (For example, (+) is such a function.)
     Suppose further that e : 'a is an identity of f, i.e.
     for all x : 'a, f e x = f x e = x.
     (For example, 0 is an identity of (+).)
     Then, for any l : 'a list, we have
     fold_right f l e = fold_left f e l

     You can't prove this theorem directly; it must be generalized.
     It is generalized in a similar way to the previous theorem, so
     try to work out the generalization yourself.
     If you get stuck, take a look at the induction practice problem
     set posted for Midterm 1. It has this problem on it, and a
     solution for it.
   *)

  (* Enough with folds. Let's see some other examples of things we can
     do with lists. *)

  (* Also from this point onwards, feel free to use 'a mylist or plain
     'a list. *)

  (* pairs : 'a list -> ('a * 'a) list
     pairs l makes a list of all successive pairs of elements in l;
     that is, it groups the elements two-by-two.

     e.g.
     pairs [x1, x2, x3, x4, x5]
     = [(x1, x2); (x2, x3); (x3, x4); (x4; x5)]

     Notice that there is one fewer element in the output than in the
     input.

     Implement this function using plain pattern matching and
     recursion.

     Rank: *
   *)
  let rec pairs l = match l with
    | (x1::x2::xs) -> (x1,x2) :: (pairs (x2::xs))
    | _ -> []

  (* Your previous implementation was almost certainly not tail
     recursive, so let's rewrite it tail-recursively using continuations.

     pairs_k : 'a list -> (('a * 'a) list -> 'r) -> 'r

     Rank: **
   *)
  let rec pairs_k l =
    let rec aux l k = match l with
      | x1::x2::xs -> aux (x2::xs) (fun s -> k ((x1,x2)::s))
      | _ -> k []
    in
    aux l (fun x -> x)


  (* I know I said no more folds, but I can't help myself.

     A _scan_ is a variant of a fold that collects all the
     intermediate results into a list. Many algorithms can be
     elegantly expressed as a scan.

     scan_left : ('b -> 'a -> 'b) -> 'b -> 'a list -> 'b list

     For example,
     scan_left (+) 0 [1;2;3;4;5]
     = [1; 3; 6; 10; 15]
     calculates the list of all the partial sums, from left to right.

     In general,
     scan_left f e [x1;x2;...]
     = [f e x1; f (f e x1) x2; f (f (f e x1) x2) x3; ...]
   *)
  let rec scan_left f acc l = match l with
    | (x::xs) ->
      let next_acc = f acc x in
      next_acc :: scan_left f next_acc xs
    | [] -> []


  (* Okay, now that's enough with lists. *)
  (* Next topic: math *)
  type exp =
    | Var of string  (* a variable, e.g. Var "x" *)
    | Lit of float (* a constant, e.g. Lit 3.0 *)
    | Plus of exp * exp (* a sum, i.e. e1 + e2 *)
    | Times of exp * exp (* a product, i.e. e1 * e2 *)
    | Exp of exp (* exponential: exp(e) *)
    | Ln of exp (* natural log: ln(e) *)

  let rec print_exp e = match e with
    | Var s -> s
    | Lit f -> string_of_float f
    | Plus (e1,e2) -> "("^(print_exp e1) ^ "+" ^ (print_exp e2) ^ ")"
    | Times (e1,e2) ->  "("^(print_exp e1) ^ "*" ^ (print_exp e2) ^ ")"
    | Exp e ->  "exp (" ^ (print_exp e) ^ ") "
    | Ln e -> "ln (" ^ (print_exp e) ^ ") "


  (* There are some things missing in our exp datatype
     but that's because they can be expressed in terms of things we
     already have. *)

  (* For instance, we don't have general exponentiation.
     We only have exponentiation with base e.
     However, notice:
     b = exp (ln b) since e and ln are inverses.
     Therefore, b^x = (exp (ln b))^x
     which rewritten = exp (x * ln b)
     and this can be expressed using the syntax tree we have defined.

     Implement the function
     exp_base : exp -> exp -> exp
     such that
     exp_base b x represents b^x by expressing it using the exp syntax
     tree as exp (x * ln b)

     Rank: *
   *)
  let exp_base b x = Exp (Times (x,Ln(b)))

  (* Use exp_base to construct the reciprocal.

     Implement the function
     recip : exp -> exp
     such that
     recip e computes 1/e

     (Recall: 1/e = e^-1)
   *)
  let recip e = exp_base e (Lit (-1.0))

  (* Computationally, it's inefficient to detour through
     the exponential and ln for integer powers.
     Instead, for integer powers, we can just use repeated
     multiplication.
     Implement the function
     pow : exp -> int -> exp
     such that
     pow e k computes e * e * e * ... * e (k times)
     You can assume `k` is positive (no need to check this).
     Rank: *
  *)


  let rec pow e k = if k = 0 then ( Lit 1.0) else Times (e,pow e (k-1))

  (* Polynomials are an important class of functions.
     We would like a convenience function for constructing polynomials
     given just a list of coefficients.

     Implement the function
     poly : string -> float list -> exp
     such that
     poly x [c0;c1;c2; ...; cn]
     constructs the polynomial
     c0 * x^0 + c1 * x^1 + c2 * x^2 + ... + cn * x^n

     Rank: **
   *)
  let poly x cs =
    let rec aux cs i acc = match cs with
      | c::xs -> aux xs (i+1) (Plus (acc, Times (Lit c,pow x i)))
      | [] -> acc
    in
    aux cs 0 (Lit 0.0)

  (* We would like to evaluate an expression.

     Assuming that an expression contains no variables, this amounts
     to just performing all the necessary calculations.

     Implement the function
     eval : exp -> float
     which evaluates a given expression which is _assumed_ to contain
     no variables. So if you encounter a variable in the expression,
     you may crash in your favourite way, e.g. failwith "oh no" or
     assert false.

     By the way, to implement the cases for Exp and Ln, you may want
     to use respectively the built-in functions
     - exp : float -> float
     - log : float -> float

     Rank: *
  *)

  exception UnboundVariable
  let rec eval e =  match e with
    | Var s -> raise UnboundVariable
    | Lit f -> f
    | Plus (e1,e2) -> (eval e1) +. (eval e2)
    | Times (e1,e2) ->(eval e1) *. (eval e2)
    | Exp e1 -> exp (eval e1)
    | Ln e1 -> log (eval e1)


  (* Sadly, not all expressions are free of variables!

     Implement the function
     subst : string * exp -> exp -> exp
     such that
     subst (x, e') e
     replaces all occurrences of the variable x in the expression e
     with e'.

     By the way, this is significantly simpler than substitution in
     MiniCaml since our language of expressions contains no binders!
     (So variable capture is impossible.)

     Rank: *
   *)
  let rec subst (e', x) e =  match e with (*Note that the order of the tuple has been swapped to be consistent with the notation [e'/x]*)
    | Var s when s=x-> e'
    | Plus (e1,e2) ->  Plus (subst (e',x) e1,subst (e',x) e2)
    | Times (e1,e2) -> Times (subst (e',x) e1, subst (e',x) e2)
    | Exp e1 -> Exp (subst (e',x) e1)
    | Ln e1 -> Ln (subst (e',x) e1)
    | x -> x

  (* Implement the function
     eval_at : string * float -> exp -> float
     such that
     eval_at (x, a) e
     performs the substitution (x, Lit a) on e, and then evaluates the
     result.

     Assume that e has only one variable, and that the substitution s
     does not introduce any new variables.

     Rank: zero stars
   *)
  let eval_at (a,x) e = (*Note that the order of the tuple has been swapped to be consistent with the notation [a/x]*)
    let e = subst (a,x) e in
    eval e

  (* Differentiation is a technique from calculus allowing us to
     determine the slope the tangent line of a given curve at a given
     point.

     I will write `Dx e` to denote "the derivative of expression e
     with respect to x".

     It is defined by the following rules:
     Dx x = 1                         -- derivative of the target variable is 1
     Dx y = 0                         -- derivative of other variables is 0
     Dx c = 0                         -- derivative of a constant is 0
     Dx (e1 + e2) = Dx e1 + Dx e2     -- linearity of differentiation
     Dx (e1 * e2)
        = e1 * (Dx e2) + (Dx e1) * e2 -- the product rule
     Dx exp(e) = exp(e) * Dx e        -- exponential + chain rule
     Dx ln(e) = (1/e) * Dx e          -- natural log + chain rule

     Implement the function
     deriv : string -> exp -> exp
     such that
     deriv x e computes the derivative of e with respect to x

     Hint: for the Ln case, you should use the recip function you
     already implemented.

     Rank: **
   *)
  let rec deriv x e =  match e with
    | Var s when x = s -> Lit 1.0
    | Plus (e1,e2) -> Plus ((deriv x e1),(deriv x e2))
    | Times (e1,e2) -> Plus (Times(e1,deriv x e2),Times(deriv x e1,e2))
    | Exp e1 -> Times (Exp e1, deriv x e1)
    | Ln e1 -> Times(deriv x e1,recip e1)
    | _ -> Lit 0.0

  (* The Newton-Raphson method is a technique for finding the roots of
     a given function.

     We say that `a` is a root of the function `f` if f(a) = 0.

     In this problem, we represent functions as expressions involving
     a variable x.

     The NR method begins with an initial guess x0 and produces a
     refinement of the guess, x1. This refinement is then used as the
     next guess, and so on, until we reach a guess x' such that f(x')
     is sufficiently close to zero.
     Consider f(x') to be sufficiently close if it is within 0.001 of
     zero.

     Concretely, the next guess x_(i+1) is calculated from this
     equation:

     x_(i+1) = x_i - (f(x_i) / f'(x_i))

     Where f' denotes the derivative of f with respect to x.

     Implement the function
     nr : float -> exp -> float
     such that
     nr x0 e = a
     computes a root `a` of the function expressed by `e` given an
     initial guess x0.

     Hint:
     Notice in the description of the problem, we need the derivative
     of f as well as f itself. To obtain the derivative, use `deriv`.
     To evaluate f and f' at specific points, use `eval_at`.

     Rank: **
   *)
  let rec nr x0 e =
    let d_e = deriv "x" e in
    let e' = eval_at (Lit x0,"x") e in
    let d_e' = eval_at (Lit x0,"x") d_e in
    let x1 = x0 -. (e'/.d_e') in
    if abs_float(eval_at (Lit x1,"x") e ) < 0.001 then
      x1
    else
      nr x1 e

  (* Are you tired of calculus? I'm tired of calculus.
     Let's talk about code generation / partial evaluation. *)

  (* The canonical example: pow

     We can calculate n^k using a simple recursive program.
   *)
  (* let rec pow k n =
   *   if k = 0 then 1
   *   else pow (k-1) n * n *)

  (* Rewrite this function so it performs partial evaluation.

     That is, implement the function
     pow_gen : int -> int -> int
     such that
     pow_gen k computes a NON-RECURSIVE function f
     such that
     f n computes n^k

     In other words, the function f that is returned must not contain
     a recursive call to pow_gen.
     By the way, this rules out the putative answer
     pow_gen k = pow k
     by just partially applying pow !

     Rank: *
   *)
  let pow_gen k =
    let rec aux k cont =
      if k = 1 then
        cont
      else
        aux (k-1) (fun x -> x * (cont x))
    in
    aux k (fun x -> x)


  (* Back to lists!

           Long ago, at the top of this file, you implemented append, for the
     mylist type. Notice in that implementation that the second list is only
     needed at the very end. This suggests that we can use partial evaluation to
     construct a non-recursive function that just waits for the second list.

           Implement the function app_gen : 'a list -> 'a list -> 'a list such
     that app_gen l1 computes a non-recursive function f such that f l2 computes
     the concatenation of l1 and l2.

           Rank: * *)
  let app_gen l1 =
    let rec aux l cont = match l with
      | x :: xs -> aux xs (fun y -> cont (x::y))
      | [] -> cont
    in
    aux l1 (fun x-> x)

  (* In the section on math, you implemented the function
     `subst` which replaces all occurrences of a specified variable
     with a given expression.

     An important thing to notice about that function is that the
     given expression to plug in is only needed when the appropriate
     Var node is encountered.
     This suggests you can use partial evaluation to construct a
     non-recursive function that waits for this expression later.

     Implement the function
     subst_gen : string -> exp -> exp
     such that
     subst_gen x e computes a non-recursive function f such that
     f e' computes the same answer as subst (x, e') e

     (Sadly, the parameters have to be switched around here to make it
     work.)

     Rank: ***
   *)
  let subst_gen x e = assert false

  (* In this problem, we consider partially evaluated polynomials.

     Implement a function
     poly_gen : int list -> int -> int
     such that
     poly_gen [c0;c1;...;cn] computes a non-recursive function f
     equivalent to
     fun x -> c0 * x^0 + c1 * x^1 + ... + cn * x^n
     crucially, the constructed function f does not contain any
     recursive call to poly_gen.

     To calculate x^k, you should use pow k x.

     Rank: **
   *)
  let poly_gen cs = match cs with
    | y::ys ->
      (fun x ->
         match
           begin
             List.fold_left
               (fun y z -> match y with
                    e,i -> (Plus (e,Times(Lit z,pow x i)),i+1)
               )
               (Lit y,1)
               ys
           end
         with (acc,_) -> acc
      )
    | [] -> assert false

  (* Consider a type of binary tree. *)
  type 'a tree =
    | Empty
    | Node of 'a tree * 'a * 'a tree

  type 'a bst = (int * 'a) tree

  (* In this problem, we consider a binary SEARCH tree, so the
     elements are ordered.
     That is, for any tree t = Node (l, (k, v), r)
     we have that every key k' in l is LESS THAN (or equal) k
     and every key k' in r in GREATER THAN (or equal) k.

     First, implement the function
     insert : 'a bst -> int -> 'a -> 'a bst
     such that
     insert t k v
     inserts the value v with the key k in the BST t.
     This should overwrite any previous value associated with k in the BST.

     Rank: *
   *)
  let rec insert t k v = match t with
    | Node (l,(k',v'),r) when k'>k -> Node (insert l k v, (k',v'), r)
    | Node (l,(k',v'),r) when k'<k -> Node (l,(k',v'),insert r k v)
    | Node (l,_,r) -> Node (l,(k,v),r)
    | Empty -> Node (Empty,(k,v),Empty)

  (* Is your implementation of insert tail recursive?
     Explain by giving the definition of a tail recursive function.

     Rank: *
   *)

  (* Now we wish to implement the opposite operation.
     Implement the function
     lookup : 'a bst -> int -> 'a option
     such that
     lookup t k
     looks up a key k in the binary search tree t, returning its
     associated value. The function should return None in case it
     can't find the given entry.

     Rank: *
   *)
  let rec lookup t k = match t with
    | Node (l,(k',v'),r) when k'>k -> lookup l k
    | Node (l,(k',v'),r) when k'<k ->lookup r k
    | Node (_,(_,v'),_) -> Some v'
    | Empty -> None

  (* Prove, using induction, the following theorem.

     For any t : 'a bst, k : int, v : 'a, we have
     lookup (insert t k v) k = Some v

     HINT:
     In the step case, you will need to consider subcases for whether
     k is less than, equal to, or greater than the key being
     considered in this node.

     I did this problem in class, so you can refer to your class notes
     if you get stuck.

     Rank: ***
   *)

  (* Notice, in your implementation of insert, you did not need the
     *value* to insert until the very end.
     Once again, we can use partial evaluation.

     Implement the function
     insert_gen : 'a bst -> int -> 'a -> 'a bst
     such that
     insert_gen t k computes a non-recursive function f
     such that f v inserts v associated to k into the tree t.

     Rank: **
   *)
  let insert_gen t k =
    let rec aux t cont = match t with
      | Node (l,(k',v'),r) when k'>k -> aux l (fun x -> cont (Node (x,(k',v'),r)))
      | Node (l,(k',v'),r) when k'<k -> aux r (fun x -> cont (Node (l,(k',v'),x)))
      | Node (l,_,r) -> (fun x -> Node (l,(k,x),r))
      | Empty -> (fun x -> cont (Node (Empty,(k,x),Empty)))
    in
    aux t (fun x -> x)

  ;;


end

module Lazy = struct
  type 'a susp = Susp of (unit -> 'a)
  let delay f = Susp f
  let force (Susp f) = f ()

  (* Usual definition of streams. *)
  type 'a str =
    { hd : 'a
    ; tl : 'a str susp
    }

  (*Helper function to view stream*)

  let rec take n s =
    if n = 0 then [] else s.hd::take (n-1) (force s.tl)

  (** Implement the function
      nth : int -> 'a str -> 'a
      such that
      nth n s computes the nth element of the stream s.

      Rank: *
   *)
  let rec nth n s = if n = 0 then s.hd else nth (n-1) (force s.tl)

  (** The fibonacci sequence is defined as
      fib 0 = 0
      fib 1 = 1
      fib n = fib (n-1) + fib (n-2)

      Rewriting the last equation by shifting n gives:
      fib (n+2) = fib (n+1) + fib n

      Construct the stream `fib` of all fibonacci numbers.

      Rank: **

      There is a "fancy" way to do this, like I tried in class, using
      higher-order functions on streams.

      HINT: There is also a more "pedestrian" way.
      Use a helper function to calculate, given fib n and fib (n+1),
      the fibonacci sequence starting from fib (n+2).
  *)

  let fib : int str =
    let rec aux n1 n2 =
      {
        hd = n1;
        tl = Susp (fun () -> aux (n2) (n1+n2))
      }
    in
    aux 0 1

  (** In mathematics, a sequence is defined as a function from the
      natural numbers to some other set. That is, for each natural
      number `n`, there is an element of the sequence a_n.

      Define the function
      seq : (int -> 'a) -> 'a str
      such that given f : int -> 'a
      (which represents a mathematical sequence)
      seq f computes the stream
      f 0; f 1; f 2; ...

      Rank: *
   *)
  let seq f =
    let rec aux n1 =
      {
        hd = f n1;
        tl = Susp (fun () -> aux (n1+1))
      }
    in
    aux 0



  (** The Wallis product, published in 1656, is an infinite product
      whose limit is pi/2. It is among the oldest known ways to
      calculate pi to arbitrary precision.

      It is defined as:
      (2/1 * 2/3) * (4/3 * 4/5) * (6/5 * 6/7) * (8/7 * 8/9) * ...

      Notice that the nth factor in the product is given by the
      formula:

      a_n = 2n/(2n-1) * 2n/(2n+1)
          = 4n^2/(4n^2 - 1)
      (Remark: this sequence begins at n=1 !)

      Denote by W_n the Wallis product truncated at factor n.
      So W_1 = a_1 = 2/1 * 2/3
      W_2 = a_2 * W_1
      W_3 = a_3 * W_2 = a_3 * a_2 * a_1
      and so on.

      https://en.wikipedia.org/wiki/Wallis_product
   *)

  (** Write a recursive function
      f : int -> float
      such that
      f n computes W_n.

      Then, use `seq f` to obtain the sequence of all approximations
      to the Wallis product.

      HINTS:
      - You will need to use float_of_int to make this work.
      - Since the wallis product starts at n=1, you will need to do
        some shifting to make it work with your `seq`, which begins
        with `f 0`.

      Rank: *
  *)

  let wallis_1 =
    let rec f n =
      if n = 0 then
        1.0
      else
        (4.0*.(float_of_int n)**2.0)/.((4.0*.(float_of_int n)**2.0)-.1.0) *. f (n-1)
    in
    force ((seq f).tl)

  (** This previous method is quite inefficient, since it recalculates
      the same things over and over again, through the recursive
      function `f`.

      Recall:
      a_n = 4n^2/(4n^2 - 1)
      and
      W_(n+1) = a_n * W_n

      Using this, directly construct the stream

      wallis_2 : float str

      which contains all successive approximations of the Wallis
      product.

      Rank: **
   *)
  let wallis_2 =
    let rec aux n an=
      let anplus1 = (4.0*.(float_of_int (n+1))**2.0)/.((4.0*.(float_of_int (n+1))**2.0)-.1.0) in
      {
        hd = an;
        tl = Susp (fun x -> aux (n+1) anplus1)
      }
    in
    aux 1 (4.0/.3.0)


  (** The super-Catalan numbers are a two-dimensional generalization
      of the Catalan numbers.

      We have this closed form equation in terms of factorials:

                (2m)! (2n)!
      C(m, n) = ------------
                (m+n)! m! n!

      Implement the function
      superc : int -> int -> int
      such that
      superc m n = C(m, n)

      Implement factorial recursively as a helper.
      Recall
      0! = 1
      (n+1)! = n! * (n + 1)
   *)
  let superc m n =
    let rec fact n = if n = 0 then 1 else n*(fact (n-1)) in
   float_of_int ((fact (2*m))*(fact (2*n))) /. float_of_int ((fact (m+n))*(fact m)*(fact n))

  (** An infinite two-dimensional grid of integers can be modelled
      with the type `int str str`.
      We can think of this as an infinite stream of infinite columns.

      Using superc, construct the infinite grid of super-Catalan
      numbers
      supercatalan : int str str
      such that
      nth m (nth n supercatalan) = superc m n = C(m, n)

      Hint: First solve the subproblem of calculating, for some fixed
      k, the stream
      column : int str
      such that
      nth m column = superc m k
      Then, generalize this to generate the sequence of all the columns.

      Rank: **
   *)
  let supercatalan =
    let rec aux1d m n =
      {
        hd = superc m n;
        tl = Susp (fun () -> aux1d m (n+1))
      }
    in
    let rec aux2d m n =
      {
        hd = aux1d m n;
        tl = Susp (fun () -> aux2d (m+1) n)
      }
    in
    aux2d 0 0

end
